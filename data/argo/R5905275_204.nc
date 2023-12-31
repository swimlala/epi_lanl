CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-30T22:31:07Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7p   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8P   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8X   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8\   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8`   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9,   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9l   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           9t   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            9�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [`   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ɠ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  р   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � `   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 7�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � g    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230730223107  20230730223107  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�>sԓ@�@�>sԓ@�11  @�>t  �@�>t  �@,cR�C��@,cR�C���cϩ*0U2�cϩ*0U211  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?�=q@�\@=p�@}p�@�  @�G�@�G�A   A  A   A+�A@��A`  A\)A�  A�Q�A�  A�  A�  A߮A�B   B(�B  B  B�
B'�B0  B8(�B@Q�BH(�BP(�BX  B`  Bh  Bp  Bx(�B�  B�  B��B�{B�{B�  B�  B�  B��B��
B�  B�  B�  B�{B��B�  B�{B��B�  B�(�B�(�B�ffB�{B�  B��B��B��B��B��B�  B�{B�{C   C  C  C  C
=C
{C  C��C��C  C
=C  C  C  C  C  C��C!��C$  C&
=C'��C*  C,  C.{C0
=C2  C4
=C6{C8
=C:  C<  C>
=C@  CA��CC��CE��CG��CJ
=CL
=CN
=CP  CR  CT  CV  CW��CY�C[��C]��C_��Ca�Cd  Ce��Cg�Ci�Ck�Cn  Cp
=Cr
=Ct
=Cv{Cx
=Cy��C{��C~  C�  C���C���C���C�C�
=C�C�  C���C���C���C�  C�  C�C�  C�  C�  C�  C���C���C���C���C�  C���C���C�  C�  C���C���C�  C�C�  C���C���C�  C�  C�  C�C�C�  C�C�C���C���C�  C�C�  C���C�  C�C�  C�  C�  C�C�C�C�C�C�  C�  C�
=C�
=C�  C���C���C�C�C���C���C�  C�  C�C���C���C���C���C���C�C�C�C�C�  C���C���C�C�  C�C�  C�  C�
=C�
=C�  C���C�  C�
=C�C�  C�C�  C���C�  C�  C���C���C���C���C���C���C���C�  C�C�  C�  C�C���C�  C�C���C�  C�  C�C�C���C�  C�  C�  C�  C�  D   D � D �qD}qD�D� D�D��D��Dz�D�qDz�D  D� D  D� D�qD��D	�D	� D
�D
��D�D��D�D}qD��D� D  D� D  D� D  D� D�qD}qD�D� D  D��D  D}qD�D� D��D}qD  D��DD� D  D� D  D�DD��DD�D�qDz�D�D� D�qD��D   D � D!D!�D!�qD"}qD"�qD#}qD$�D$� D%  D%��D&  D&� D'  D'� D(  D(� D)�D)� D)��D*}qD*��D+}qD+�qD,� D-  D-��D.�D.��D/�D/}qD0  D0��D0�qD1� D2�D2� D2�qD3� D4�D4}qD4��D5}qD6  D6}qD6�qD7}qD8  D8��D9  D9}qD9�RD:}qD;�D;� D<  D<}qD<�qD=z�D=�qD>}qD>��D?� D@D@� D@�qDA��DB  DBz�DB�qDC� DD  DD� DE�DE��DF  DF� DF�qDG� DG�qDH}qDI  DI� DI�qDJz�DJ��DK��DL�DL��DM�DM}qDM��DN� DO  DO}qDO�qDP}qDP�qDQ}qDR  DR}qDR�qDS}qDT  DT��DU�DU��DV�DV� DW  DW}qDW��DX}qDX�qDY}qDZ  DZ� D[  D[��D\�D\}qD\��D]}qD]�qD^}qD_  D_��D`  D`z�D`�qDa� Da�qDb}qDc  Dc� Dc�qDd}qDd��De� Df  Df� Dg�Dg� Dh  Dh}qDh�qDi� Dj  Dj� Dk�Dk��Dl�Dl��Dm  Dm� Dm�qDnz�Do  Do� Do�qDp}qDp�qDq� Dr  Dr� Ds  Ds� Dt�Dt}qDt�qDu��Dv�Dv� Dw  Dw��Dw�qDx}qDy  Dy� Dz  Dz� D{  D{� D|�D|� D}  D}}qD}�qD~� D  D��D�  D�>�D�}qD���D�  D�@ D�� D�� D�  D�>�D�� D�� D�HD�AHD�� D���D�  D�@ D�~�D�� D�HD�AHD�� D���D���D�>�D�~�D���D���D�>�D�� D�� D�HD�@ D�~�D�� D�  D�@ D�� D��qD���D�>�D�� D�� D�  D�>�D�� D���D���D�>�D�~�D��HD�  D�>�D�~�D�� D���D�@ D���D��HD�HD�@ D��HD��HD�HD�AHD�� D���D��qD�>�D��HD�� D��qD�>�D��HD�� D�  D�AHD�� D�� D�  D�@ D��HD��HD�HD�>�D�� D��HD���D�@ D��HD�� D�HD�B�D��HD�� D�  D�>�D�}qD��qD���D�@ D�� D���D�  D�@ D��HD�� D�  D�@ D��HD�� D�  D�AHD�~�D���D�HD�AHD��HD��HD�  D�AHD�� D�� D�HD�>�D�� D���D�HD�>�D�� D�� D�HD�B�D�� D��)D��qD�>�D�� D�� D���D�@ D���D��HD��qD�=qD�~�D���D���D�>�D�~�D�� D�HD�AHD��HD��HD�HD�AHD�� D��qD���D�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�~�D��HD�  D�@ D�� D���D�  D�B�D��HD�� D���D�@ D��HD�� D�HD�AHD��HD�� D�  D�AHD�� D���D�  D�AHD�� D�� D�  D�B�D�� D�� D�HD�@ D�~�D��HD�  D�>�D�}qD�� D�HD�AHD�� D���D��)D�>�D��HD�� D�  D�@ D�� D���D���D�>�D�� D�� D�HD�@ D��HD��HD�HD�AHD���D�D��D�AHD�~�D���D���D�@ D�~�D��qD���D�=qD��HD���D�HD�AHD��HD��HD���D�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D�}qD���D�  D�@ D D�� D�  D�@ D�~�D�� D�HD�@ D�}qDľ�D�HD�AHD�~�D�� D�HD�AHDƀ D�� D�  D�@ Dǀ D�� D�  D�@ D�~�D�� D�  D�>�Dɀ D�� D�  D�AHDʀ D�� D�  D�@ D�~�D˾�D���D�>�D�~�D�� D���D�>�D̀ D�� D���D�@ D΁HD�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�AHDр D�� D�HD�AHDҁHD��HD�HD�AHDӁHD��HD�  D�@ DԀ D�� D���D�>�DՀ D�� D���D�>�Dր D��HD�HD�B�DׁHD��HD�  D�@ D؀ D�� D�  D�@ Dـ Dپ�D�  D�>�D�~�D��HD�HD�>�D�~�D��HD�HD�@ D܀ D��HD�  D�>�D�~�DݽqD���D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��HD�HD�AHD�HD�D�HD�>�D� D��HD�  D�>�D�~�D㾸D�  D�AHD�~�D侸D�HD�AHD� D�� D�HD�@ D� D�� D�  D�B�D�HD��HD�HD�@ D�~�D�� D�HD�>�D� D��HD�HD�AHD�HD�� D�  D�>�D�~�D뾸D���D�@ D�~�D쾸D���D�@ D�HD��HD���D�=qD� D�� D���D�>�D�HD��HD�  D�AHD��HD�� D���D�>�D� D��HD�  D�@ D�HD�D���D�>�D�~�D�D���D�AHD�HD��HD�HD�AHD��HD���D���D�>�D�~�D���D���D�AHD��HD��HD�HD�AHD��HD�� D���D�>�D��HD��HD�HD�&fD�b�?#�
?L��?u?��
?�p�?�ff?��H@�@&ff@0��@E�@Y��@h��@z�H@��@�33@��H@��@�\)@�Q�@\@�\)@ٙ�@�\@�{@�Q�A ��AffA(�AG�AA�A!�A%A*�HA1�A6ffA:�HA@��AFffAI��AO\)AUAZ=qA^{Ac�
Ah��Al��Aq�Aw
=A{�A�  A��\A���A��RA���A��
A�A�Q�A��HA��A�
=A��A�z�A�{A���A��A�A��A�=qA�z�A�ffA�G�A��A�A�  A��HA��A�\)A��A�(�A�{Aȣ�A�33A���A�\)A��AӅA�{Aأ�A��HA���A�\)AᙚA�A�A�Q�A�\A�z�A�RA�A�A�p�A�Q�A��\A���A�
=B ��B{B
=BQ�B��B�RB�
B	�B
{B33Bz�B��B�\B(�BG�B=qB�B��B�B
=BQ�Bp�B�RB�
BG�BffB\)B ��B"{B#33B$Q�B%B'33B((�B)p�B*�HB,(�B-G�B.�RB0  B1�B2ffB3�
B5�B6{B7�B8��B9�B;33B<��B=�B?
=B@Q�BABB�HBD  BEG�BF�RBH  BH��BJ�\BK�
BL��BNffBO�
BQG�BRffBS�
BUp�BV�\BW�
BYG�BZ�HB\  B]p�B_
=B`Q�Ba��Bc33Bd��BeBg33Bh��Bj{Bk\)Bl��BnffBo�Bq�Br�\Bt  BuG�Bv�RBxQ�By��Bz�HB|(�B}B
=B�{B��HB���B�=qB��HB��B�ffB�
=B��B�Q�B��B��
B�z�B��B��B��\B�33B��B���B�\)B��B��RB�p�B�{B��RB�\)B�(�B��HB�p�B�(�B���B���B�=qB���B��B�z�B��B�B�z�B�G�B�  B��RB�\)B�(�B���B�B�z�B�33B�{B���B��B�ffB�33B�{B��HB��B�Q�B��B�  B��RB�p�B�=qB�
=B��
B��\B�33B�  B���B��B�Q�B�
=B��
B���B�p�B�(�B��HB���B�z�B�G�B�{B��RB��B�Q�B��B�  B���B�p�B�=qB�
=B��
B£�B�G�B�{B��HB�B�z�B�33B��BȸRBɅB�ffB��B�B�z�B�33B��BθRB�p�B�  BУ�B�\)B�  B�z�B���B�p�B�  B�ffB���B��B�G�BՅB��
B�(�B�Q�B�ffB֏\B��HB��B�33B�G�BׅB��
B�  B�(�B�=qB�ffBظRB��HB�
=B�33B�G�BمBٮB��B�{B�=qB�Q�B�z�Bڣ�B��HB��B�G�B�\)B�p�Bۙ�B��
B�{B�=qB�ffB�ffBܣ�B���B�
=B�G�B�\)B�p�Bݙ�B��
B�{B�=qB�ffBޏ\B��HB��B�G�B߅Bߙ�B��
B�{B�ffB��B���B���B��B�\)BᙚB��
B�{B�=qB�z�B��B��HB�33B�p�B�B��B�{B�Q�B�z�B�RB�
=B�\)B噚B��
B�{B�=qB�z�B�RB�
=B�\)B癚B��B�(�B�ffB�\B��HB��B�p�B�B�{B�Q�B��B���B�33B�B�B�{B�ffB�RB�
=B�\)B�B�{B�z�B���B��B�B��
B�(�B�z�B��HB�G�B�B�  B�z�B��HB�\)B�B�(�B�\B���B�p�B�B�(�B��\B���B�\)B��
B�=qB���B�
=B��B��B�ffB���B�33B��B�(�B���B�
=B�p�B�  B�ffB���B�G�B��C 
=C G�C �C �C �C(�C\)C��C��C
=C=qCz�C�RC�C(�C\)C��C��C  C=qCz�C�C�HC{CQ�C�\CC��C(�C\)C��C��C  C33Cp�C��C�HC�CQ�C�C�RC��C	(�C	\)C	�\C	�RC	��C
�C
\)C
�\C
�RC
�C�CG�C�C�RC�HC{CG�C�C�C�HC{CG�Cz�C�C�HC{CG�Cz�C�C�HC{CG�Cz�C�C�HC�CQ�C�C�RC�C{C\)C�\CC  C33CffC�\CC��C(�C\)C�\C�RC��C(�C\)C�\CC��C33CffC��C�
C  C(�C\)C�\CC��C�CQ�Cz�C�C�HC�CQ�C�C�RC�C�CG�Cp�C��C�
C
=C=qCz�C�C�HC{CG�Cp�C��C��C  C33CffC��C�HC{CQ�Cz�C�C�HC{CG�Cz�C�RC�HC�CQ�C�C�RC��C (�C \)C ��C ��C!
=C!=qC!p�C!�C!�HC"{C"G�C"z�C"�C"�C#�C#Q�C#�\C#��C#��C$33C$z�C$�C$�C%(�C%\)C%��C%C&  C&(�C&\)C&��C&��C'  C'=qC'z�C'�C'�HC(
=C(G�C(z�C(�C(�C)(�C)ffC)��C)�
C*{C*G�C*z�C*�C*�HC+�C+\)C+��C+�HC,�C,Q�C,�C,�RC,��C-33C-p�C-�RC-�C.(�C.ffC.��C.��C/  C/33C/z�C/�RC/�C0(�C0ffC0��C0�
C1
=C1G�C1z�C1C1��C2=qC2z�C2�RC2��C333C3ffC3��C3�HC4�C4ffC4�C4�C5(�C5p�C5��C5�
C6�C6\)C6��C6�HC7�C7\)C7��C7�
C8{C8Q�C8��C8�HC9�C9G�C9��C9�HC:(�C:p�C:�C:�C;(�C;p�C;�RC<  C<G�C<�\C<C=
=C=Q�C=��C=�C>33C>z�C>C>��C?=qC?�C?��C@{C@ffC@��C@�CA(�CAffCA�CA��CBG�CB�\CB��CC
=CCG�CC�\CC��CD�CDffCD�CD�CE(�CEp�CECF{CFQ�CF��CF�
CG�CG\)CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            ?�=q@�\@=p�@}p�@�  @�G�@�G�A   A  A   A+�A@��A`  A\)A�  A�Q�A�  A�  A�  A߮A�B   B(�B  B  B�
B'�B0  B8(�B@Q�BH(�BP(�BX  B`  Bh  Bp  Bx(�B�  B�  B��B�{B�{B�  B�  B�  B��B��
B�  B�  B�  B�{B��B�  B�{B��B�  B�(�B�(�B�ffB�{B�  B��B��B��B��B��B�  B�{B�{C   C  C  C  C
=C
{C  C��C��C  C
=C  C  C  C  C  C��C!��C$  C&
=C'��C*  C,  C.{C0
=C2  C4
=C6{C8
=C:  C<  C>
=C@  CA��CC��CE��CG��CJ
=CL
=CN
=CP  CR  CT  CV  CW��CY�C[��C]��C_��Ca�Cd  Ce��Cg�Ci�Ck�Cn  Cp
=Cr
=Ct
=Cv{Cx
=Cy��C{��C~  C�  C���C���C���C�C�
=C�C�  C���C���C���C�  C�  C�C�  C�  C�  C�  C���C���C���C���C�  C���C���C�  C�  C���C���C�  C�C�  C���C���C�  C�  C�  C�C�C�  C�C�C���C���C�  C�C�  C���C�  C�C�  C�  C�  C�C�C�C�C�C�  C�  C�
=C�
=C�  C���C���C�C�C���C���C�  C�  C�C���C���C���C���C���C�C�C�C�C�  C���C���C�C�  C�C�  C�  C�
=C�
=C�  C���C�  C�
=C�C�  C�C�  C���C�  C�  C���C���C���C���C���C���C���C�  C�C�  C�  C�C���C�  C�C���C�  C�  C�C�C���C�  C�  C�  C�  C�  D   D � D �qD}qD�D� D�D��D��Dz�D�qDz�D  D� D  D� D�qD��D	�D	� D
�D
��D�D��D�D}qD��D� D  D� D  D� D  D� D�qD}qD�D� D  D��D  D}qD�D� D��D}qD  D��DD� D  D� D  D�DD��DD�D�qDz�D�D� D�qD��D   D � D!D!�D!�qD"}qD"�qD#}qD$�D$� D%  D%��D&  D&� D'  D'� D(  D(� D)�D)� D)��D*}qD*��D+}qD+�qD,� D-  D-��D.�D.��D/�D/}qD0  D0��D0�qD1� D2�D2� D2�qD3� D4�D4}qD4��D5}qD6  D6}qD6�qD7}qD8  D8��D9  D9}qD9�RD:}qD;�D;� D<  D<}qD<�qD=z�D=�qD>}qD>��D?� D@D@� D@�qDA��DB  DBz�DB�qDC� DD  DD� DE�DE��DF  DF� DF�qDG� DG�qDH}qDI  DI� DI�qDJz�DJ��DK��DL�DL��DM�DM}qDM��DN� DO  DO}qDO�qDP}qDP�qDQ}qDR  DR}qDR�qDS}qDT  DT��DU�DU��DV�DV� DW  DW}qDW��DX}qDX�qDY}qDZ  DZ� D[  D[��D\�D\}qD\��D]}qD]�qD^}qD_  D_��D`  D`z�D`�qDa� Da�qDb}qDc  Dc� Dc�qDd}qDd��De� Df  Df� Dg�Dg� Dh  Dh}qDh�qDi� Dj  Dj� Dk�Dk��Dl�Dl��Dm  Dm� Dm�qDnz�Do  Do� Do�qDp}qDp�qDq� Dr  Dr� Ds  Ds� Dt�Dt}qDt�qDu��Dv�Dv� Dw  Dw��Dw�qDx}qDy  Dy� Dz  Dz� D{  D{� D|�D|� D}  D}}qD}�qD~� D  D��D�  D�>�D�}qD���D�  D�@ D�� D�� D�  D�>�D�� D�� D�HD�AHD�� D���D�  D�@ D�~�D�� D�HD�AHD�� D���D���D�>�D�~�D���D���D�>�D�� D�� D�HD�@ D�~�D�� D�  D�@ D�� D��qD���D�>�D�� D�� D�  D�>�D�� D���D���D�>�D�~�D��HD�  D�>�D�~�D�� D���D�@ D���D��HD�HD�@ D��HD��HD�HD�AHD�� D���D��qD�>�D��HD�� D��qD�>�D��HD�� D�  D�AHD�� D�� D�  D�@ D��HD��HD�HD�>�D�� D��HD���D�@ D��HD�� D�HD�B�D��HD�� D�  D�>�D�}qD��qD���D�@ D�� D���D�  D�@ D��HD�� D�  D�@ D��HD�� D�  D�AHD�~�D���D�HD�AHD��HD��HD�  D�AHD�� D�� D�HD�>�D�� D���D�HD�>�D�� D�� D�HD�B�D�� D��)D��qD�>�D�� D�� D���D�@ D���D��HD��qD�=qD�~�D���D���D�>�D�~�D�� D�HD�AHD��HD��HD�HD�AHD�� D��qD���D�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�~�D��HD�  D�@ D�� D���D�  D�B�D��HD�� D���D�@ D��HD�� D�HD�AHD��HD�� D�  D�AHD�� D���D�  D�AHD�� D�� D�  D�B�D�� D�� D�HD�@ D�~�D��HD�  D�>�D�}qD�� D�HD�AHD�� D���D��)D�>�D��HD�� D�  D�@ D�� D���D���D�>�D�� D�� D�HD�@ D��HD��HD�HD�AHD���D�D��D�AHD�~�D���D���D�@ D�~�D��qD���D�=qD��HD���D�HD�AHD��HD��HD���D�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D�}qD���D�  D�@ D D�� D�  D�@ D�~�D�� D�HD�@ D�}qDľ�D�HD�AHD�~�D�� D�HD�AHDƀ D�� D�  D�@ Dǀ D�� D�  D�@ D�~�D�� D�  D�>�Dɀ D�� D�  D�AHDʀ D�� D�  D�@ D�~�D˾�D���D�>�D�~�D�� D���D�>�D̀ D�� D���D�@ D΁HD�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�AHDр D�� D�HD�AHDҁHD��HD�HD�AHDӁHD��HD�  D�@ DԀ D�� D���D�>�DՀ D�� D���D�>�Dր D��HD�HD�B�DׁHD��HD�  D�@ D؀ D�� D�  D�@ Dـ Dپ�D�  D�>�D�~�D��HD�HD�>�D�~�D��HD�HD�@ D܀ D��HD�  D�>�D�~�DݽqD���D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��HD�HD�AHD�HD�D�HD�>�D� D��HD�  D�>�D�~�D㾸D�  D�AHD�~�D侸D�HD�AHD� D�� D�HD�@ D� D�� D�  D�B�D�HD��HD�HD�@ D�~�D�� D�HD�>�D� D��HD�HD�AHD�HD�� D�  D�>�D�~�D뾸D���D�@ D�~�D쾸D���D�@ D�HD��HD���D�=qD� D�� D���D�>�D�HD��HD�  D�AHD��HD�� D���D�>�D� D��HD�  D�@ D�HD�D���D�>�D�~�D�D���D�AHD�HD��HD�HD�AHD��HD���D���D�>�D�~�D���D���D�AHD��HD��HD�HD�AHD��HD�� D���D�>�D��HD��HD�HD�&fD�b�?#�
?L��?u?��
?�p�?�ff?��H@�@&ff@0��@E�@Y��@h��@z�H@��@�33@��H@��@�\)@�Q�@\@�\)@ٙ�@�\@�{@�Q�A ��AffA(�AG�AA�A!�A%A*�HA1�A6ffA:�HA@��AFffAI��AO\)AUAZ=qA^{Ac�
Ah��Al��Aq�Aw
=A{�A�  A��\A���A��RA���A��
A�A�Q�A��HA��A�
=A��A�z�A�{A���A��A�A��A�=qA�z�A�ffA�G�A��A�A�  A��HA��A�\)A��A�(�A�{Aȣ�A�33A���A�\)A��AӅA�{Aأ�A��HA���A�\)AᙚA�A�A�Q�A�\A�z�A�RA�A�A�p�A�Q�A��\A���A�
=B ��B{B
=BQ�B��B�RB�
B	�B
{B33Bz�B��B�\B(�BG�B=qB�B��B�B
=BQ�Bp�B�RB�
BG�BffB\)B ��B"{B#33B$Q�B%B'33B((�B)p�B*�HB,(�B-G�B.�RB0  B1�B2ffB3�
B5�B6{B7�B8��B9�B;33B<��B=�B?
=B@Q�BABB�HBD  BEG�BF�RBH  BH��BJ�\BK�
BL��BNffBO�
BQG�BRffBS�
BUp�BV�\BW�
BYG�BZ�HB\  B]p�B_
=B`Q�Ba��Bc33Bd��BeBg33Bh��Bj{Bk\)Bl��BnffBo�Bq�Br�\Bt  BuG�Bv�RBxQ�By��Bz�HB|(�B}B
=B�{B��HB���B�=qB��HB��B�ffB�
=B��B�Q�B��B��
B�z�B��B��B��\B�33B��B���B�\)B��B��RB�p�B�{B��RB�\)B�(�B��HB�p�B�(�B���B���B�=qB���B��B�z�B��B�B�z�B�G�B�  B��RB�\)B�(�B���B�B�z�B�33B�{B���B��B�ffB�33B�{B��HB��B�Q�B��B�  B��RB�p�B�=qB�
=B��
B��\B�33B�  B���B��B�Q�B�
=B��
B���B�p�B�(�B��HB���B�z�B�G�B�{B��RB��B�Q�B��B�  B���B�p�B�=qB�
=B��
B£�B�G�B�{B��HB�B�z�B�33B��BȸRBɅB�ffB��B�B�z�B�33B��BθRB�p�B�  BУ�B�\)B�  B�z�B���B�p�B�  B�ffB���B��B�G�BՅB��
B�(�B�Q�B�ffB֏\B��HB��B�33B�G�BׅB��
B�  B�(�B�=qB�ffBظRB��HB�
=B�33B�G�BمBٮB��B�{B�=qB�Q�B�z�Bڣ�B��HB��B�G�B�\)B�p�Bۙ�B��
B�{B�=qB�ffB�ffBܣ�B���B�
=B�G�B�\)B�p�Bݙ�B��
B�{B�=qB�ffBޏ\B��HB��B�G�B߅Bߙ�B��
B�{B�ffB��B���B���B��B�\)BᙚB��
B�{B�=qB�z�B��B��HB�33B�p�B�B��B�{B�Q�B�z�B�RB�
=B�\)B噚B��
B�{B�=qB�z�B�RB�
=B�\)B癚B��B�(�B�ffB�\B��HB��B�p�B�B�{B�Q�B��B���B�33B�B�B�{B�ffB�RB�
=B�\)B�B�{B�z�B���B��B�B��
B�(�B�z�B��HB�G�B�B�  B�z�B��HB�\)B�B�(�B�\B���B�p�B�B�(�B��\B���B�\)B��
B�=qB���B�
=B��B��B�ffB���B�33B��B�(�B���B�
=B�p�B�  B�ffB���B�G�B��C 
=C G�C �C �C �C(�C\)C��C��C
=C=qCz�C�RC�C(�C\)C��C��C  C=qCz�C�C�HC{CQ�C�\CC��C(�C\)C��C��C  C33Cp�C��C�HC�CQ�C�C�RC��C	(�C	\)C	�\C	�RC	��C
�C
\)C
�\C
�RC
�C�CG�C�C�RC�HC{CG�C�C�C�HC{CG�Cz�C�C�HC{CG�Cz�C�C�HC{CG�Cz�C�C�HC�CQ�C�C�RC�C{C\)C�\CC  C33CffC�\CC��C(�C\)C�\C�RC��C(�C\)C�\CC��C33CffC��C�
C  C(�C\)C�\CC��C�CQ�Cz�C�C�HC�CQ�C�C�RC�C�CG�Cp�C��C�
C
=C=qCz�C�C�HC{CG�Cp�C��C��C  C33CffC��C�HC{CQ�Cz�C�C�HC{CG�Cz�C�RC�HC�CQ�C�C�RC��C (�C \)C ��C ��C!
=C!=qC!p�C!�C!�HC"{C"G�C"z�C"�C"�C#�C#Q�C#�\C#��C#��C$33C$z�C$�C$�C%(�C%\)C%��C%C&  C&(�C&\)C&��C&��C'  C'=qC'z�C'�C'�HC(
=C(G�C(z�C(�C(�C)(�C)ffC)��C)�
C*{C*G�C*z�C*�C*�HC+�C+\)C+��C+�HC,�C,Q�C,�C,�RC,��C-33C-p�C-�RC-�C.(�C.ffC.��C.��C/  C/33C/z�C/�RC/�C0(�C0ffC0��C0�
C1
=C1G�C1z�C1C1��C2=qC2z�C2�RC2��C333C3ffC3��C3�HC4�C4ffC4�C4�C5(�C5p�C5��C5�
C6�C6\)C6��C6�HC7�C7\)C7��C7�
C8{C8Q�C8��C8�HC9�C9G�C9��C9�HC:(�C:p�C:�C:�C;(�C;p�C;�RC<  C<G�C<�\C<C=
=C=Q�C=��C=�C>33C>z�C>C>��C?=qC?�C?��C@{C@ffC@��C@�CA(�CAffCA�CA��CBG�CB�\CB��CC
=CCG�CC�\CC��CD�CDffCD�CD�CE(�CEp�CECF{CFQ�CF��CF�
CG�CG\)CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�S�A�Q�A�Q�A�Q�A�S�A�Q�A�O�A�M�A�M�A�O�A�O�A�Q�A�Q�A�S�A�VA�XA�S�A�S�A�Q�A�Q�A�Q�A�S�A�VA�VA�VA�XA�XA�XA�XA�ZA�ZA�\)A�\)A�ZA�ZA�ZA�\)A�\)A�VA�?}A�
=A�VAӝ�A�\)A�  A��A�t�A�\)A��A�33AƏ\Aę�A�
=A�ZA�A�ffA�E�A�n�A�p�A���A��jA��A�z�A�1A�E�A��A��hA�ȴA�5?A��\A�7LA�n�A��A���A�|�A��-A�E�A�;dA�K�A�ȴA�^5A��A��\A��wA��wA���A�n�A|Q�Ay�Aw
=AqAnffAl�\Akl�AhA�Ae�^Aa��A`�A]
=AX�AW`BAV�9AV1'AUG�AQ��AN��AL  AIC�AD�AA�
A@(�A?S�A>�A> �A<�A:��A8�RA7�^A6��A5;dA4��A4ȴA4��A4�\A4�uA4�+A4n�A4I�A3��A3�A3+A2��A1��A0�`A0=qA0��A1%A/A.��A.JA.��A.��A-��A-�A-?}A,�A+ƨA+�7A*�A*z�A)�A)x�A(�HA'�;A'l�A%t�A$5?A$v�A$$�A#�FA#?}A"�`A"�!A"=qA!��A �A��A?}Ap�A�wA;dA1A�A�HAJA��A�A�HAjA�
AhsA33A��A�A�A?}A?}A��A(�A1A�^A��A�AXAA�DAr�A^5A-A�-A��A�A�A��A��Az�AVA�A��A+A�+AZA=qA�mA�A�yA�A�AVA�DAE�A�A$�AbA�#A|�AO�A%A
�`A
��A
��A
r�A
M�A
 �A	�TA	��A	C�A	%A��Az�A  A��A33A��AȴA��An�A(�A�-AhsA?}A"�A��AA�A�
Al�AoA��A�Ax�AVA ��A @��
@�33@�M�@��T@��@�?}@��@��@��D@� �@�|�@���@�{@���@��F@�;d@�ff@�V@�1'@�b@�C�@��@��@�7@�@�p�@���@�  @�ff@���@�x�@�G�@�&�@��@�u@�K�@��y@�=q@�@�7L@�9@�D@� �@��H@��T@�j@�t�@�o@�ȴ@�$�@�h@�x�@�hs@�j@� �@ߕ�@�S�@�o@�5?@���@�x�@��@�z�@�9X@�1@�  @��@�v�@�$�@�hs@ؼj@ؓu@�b@�\)@�$�@�X@ԓu@�I�@���@�C�@Ұ!@��@��@ѩ�@�x�@�7L@�V@���@Ь@�z�@�j@��@��
@ύP@�K�@�n�@�7L@�(�@�C�@���@�V@�-@��@ə�@���@��@ǝ�@�+@Ɨ�@�=q@ź^@�7L@���@�Q�@�\)@�@�{@��-@�O�@���@��j@�I�@� �@���@�+@�~�@��#@�V@�Z@�b@��w@�t�@�
=@��+@��@���@�&�@�bN@�A�@��;@�K�@��y@��!@�n�@�=q@�{@�@�x�@��@��@�j@��@��w@�l�@��@�-@��#@�x�@�G�@�7L@�V@��`@���@��@�  @�dZ@���@��H@��@��\@�J@���@�hs@�&�@��@���@��j@�A�@�b@���@�33@�"�@��y@���@���@�v�@�$�@���@���@�p�@�&�@��9@�j@��@��@��@�+@�ȴ@���@�@�7L@��@��D@���@�ƨ@���@��@��!@�E�@�5?@�J@��-@�x�@��@��9@��@��D@�Z@� �@��P@�"�@��@���@�ff@�$�@��T@��@�G�@�&�@���@���@��u@� �@��;@���@�33@�ȴ@���@��\@�$�@�`B@��/@�Z@��@�t�@�o@��H@���@�n�@��@��h@�G�@���@���@��@�r�@�1@��
@�S�@��@��@��!@�~�@�-@�{@��T@��@��@���@�r�@��;@���@�33@��y@��\@�E�@�J@���@���@�p�@�X@�G�@��@�Ĝ@��D@�9X@��w@�dZ@��@���@��y@��@���@�~�@�M�@�{@���@���@���@��7@�hs@�/@���@���@�z�@�I�@�b@��F@�\)@�C�@�;d@�+@�
=@���@��\@�E�@�{@��T@���@�p�@�G�@���@��@�r�@��@��@�ƨ@�\)@�33@�
=@��y@�ȴ@��!@��+@�V@���@�`B@��@��u@�1'@�b@��@|�@+@~��@~5?@}�T@}�h@|��@|�D@|j@|Z@|1@{�@{"�@z��@z��@y��@y��@yG�@y%@y%@x��@xĜ@xQ�@w�P@v�y@v�+@v$�@u�T@u�h@u�@tz�@t(�@s��@s�m@s�
@s�F@sC�@r�!@r~�@r^5@q��@qhs@q7L@p��@pr�@p  @o�P@o�@nȴ@n$�@m�h@mV@l(�@k��@k�@kdZ@kC�@j^5@i��@iX@i7L@i&�@h��@hr�@g\)@f�y@f�+@fff@f$�@e�h@e�@d��@dZ@c��@c@b��@bn�@aG�@`�u@`�@`bN@` �@_\)@^�R@^v�@^ff@]�@]p�@]`B@]�@\�/@\�j@\z�@[��@[dZ@[@Z�H@Z�\@Y�#@Yx�@Y7L@Y�@X��@X�`@X��@X�@XbN@W�@Wl�@W+@Vȴ@V�R@Vv�@V5?@V@U��@U�@U/@Tj@S�F@SC�@So@R�H@R~�@RJ@Q��@Qhs@Q&�@Pr�@P �@O�P@O
=@N��@N@M?}@L�/@L��@L(�@K�F@KS�@K33@K@J��@J~�@J�@I�7@H�`@H�u@HbN@G��@G;d@F�y@F�+@F@E�@D�j@DI�@D(�@C�@C@B��@Bn�@B=q@A�#@A�7@A�@@Ĝ@@��@@1'@?�w@>�R@>��@>�+@>V@>$�@=�-@=p�@=?}@<��@<�@;�
@;t�@;C�@;@:�!@:�\@:=q@9�@9�@9�#@9��@9��@9hs@97L@8�`@8��@8bN@81'@7�;@7�@7��@7l�@6�y@6��@6v�@65?@6@5��@5�@4��@4��@4j@4Z@4�@3��@3dZ@3"�@2�H@2��@2-@1�#@17L@0��@0A�@/�;@/��@/|�@/�@.�R@.�+@.$�@-��@-��@-p�@-�@,�/@,�@,z�@,I�@,1@+��@+t�@+S�@+"�@*�@*�H@*��@*�\@*n�@*M�@*-@)��@)�^@)hs@)%@(��@(�u@(bN@( �@'�@'|�@';d@'�@&��@&ȴ@&ff@%�@%�@%`B@%O�@%O�@%V@$�@$�j@$z�@$1@#�@#dZ@#33@#@"�H@"��@"-@!��@!��@!X@!�@!%@ ��@ ��@ r�@  �@ b@�@��@K�@+@
=@ȴ@��@�+@v�@ff@ff@5?@{@��@��@�h@�h@�h@�@`B@/@��@�j@�D@�@�F@��@dZ@S�@C�@C�@C�@33@�@�H@�!@n�@M�@-@�@�@��@�#@�^@x�@hs@hs@X@X@�@Ĝ@��@�u@ �@�@��@�@l�@K�@+@+@�@
=@
=@�R@��@��@�+@�+@ff@5?@$�@�T@��@�@p�@p�@p�@O�@��@�/@z�@I�@9X@�@�m@�F@�@33@"�@�@��@�!@�\@n�@=q@��@��@hs@x�@hs@7L@&�@��@�9@��@�u@�@r�@�A�Q�A�VA�O�A�VA�S�A�O�A�S�A�S�A�M�A�O�A�S�A�S�A�O�A�Q�A�VA�Q�A�Q�A�VA�S�A�O�A�S�A�O�A�M�A�M�A�Q�A�K�A�K�A�Q�A�O�A�K�A�M�A�S�A�M�A�K�A�O�A�Q�A�M�A�M�A�S�A�Q�A�O�A�S�A�S�A�O�A�Q�A�Q�A�M�A�O�A�XA�VA�Q�A�VA�XA�S�A�S�A�XA�VA�S�A�XA�XA�S�A�VA�ZA�XA�VA�ZA�XA�S�A�VA�ZA�S�A�VA�XA�O�A�O�A�VA�S�A�O�A�S�A�VA�O�A�S�A�VA�Q�A�O�A�VA�S�A�O�A�Q�A�Q�A�M�A�O�A�S�A�Q�A�O�A�S�A�S�A�O�A�O�A�VA�S�A�O�A�S�A�VA�Q�A�Q�A�VA�XA�S�A�S�A�XA�VA�VA�XA�VA�Q�A�XA�VA�S�A�XA�XA�S�A�S�A�XA�XA�S�A�VA�ZA�S�A�VA�ZA�ZA�S�A�VA�ZA�XA�VA�XA�ZA�XA�S�A�ZA�ZA�VA�VA�ZA�ZA�VA�XA�ZA�VA�VA�ZA�XA�VA�ZA�\)A�XA�XA�\)A�ZA�VA�XA�\)A�\)A�XA�ZA�^5A�XA�ZA�\)A�^5A�ZA�ZA�^5A�\)A�ZA�ZA�^5A�^5A�ZA�\)A�^5A�ZA�ZA�^5A�\)A�ZA�\)A�\)A�VA�VA�ZA�XA�XA�^5A�\)A�XA�ZA�^5A�^5A�ZA�XA�^5A�^5A�ZA�ZA�`BA�^5A�\)A�ZA�ZA�VA�M�A�I�A�M�A�G�A�C�A�9XA�33A�1'A��A�VA�1A�%A�A��A�ƨAԋDA�K�A�JA���A��;A���A�ȴA�AӋDA�&�A�  A��/A�z�A�;dA�AхA�z�A�hsA���AмjA�x�A�A�A�33A�1A���Aϝ�AΡ�Aͥ�A�`BA�=qA��A��
A�ƨA̩�ÃA���A�?}AʋDA�1'A��#A���Aɺ^AɬAɣ�A�z�A�%A�^5A��
A�5?Aƺ^A�33AŇ+A�bA��
Aę�A�l�A�VA�A�A�-A��A�1A��AþwAÇ+A�I�A�K�A�K�A�5?A�&�A�{A���A��A���A®A�x�A�XA�?}A�7LA��A�ffA�$�A��A��A�{A��A��wA��PA�\)A���A���A�;dA��yA��A��A��A��A���A�oA���A�VA�+A���A�l�A�-A� �A�bA���A��A���A��wA��A���A��PA��A�x�A�O�A�"�A�{A�%A��A���A��A���A��7A��+A�t�A�l�A�bNA�M�A�=qA�-A�$�A��A���A��TA���A���A�r�A�E�A�7LA��A���A��yA��#A�ȴA��+A�Q�A�=qA� �A���A�bNA�A��-A�ffA���A��PA�v�A�O�A��A�  A���A�~�A�M�A�+A�oA�A���A��A��`A��/A��A���A��^A���A��A�^5A�33A��A�1A��A��#A��^A���A��A�p�A�S�A�C�A�1'A� �A�A��A��;A���A�A��FA���A��+A�r�A�I�A�-A�1A���A���A�VA�$�A���A���A�|�A�5?A�bA�A���A��TA��
A�ƨA��jA�Q�A��
A�hsA���A�jA�hsA� �A��TA��jA���A�x�A�p�A�C�A�+A���A��A��/A���A���A�ȴA���A��jA��RA��!A���A��PA�^5A�7LA��A�(�A��A�"�A�"�A���A��A���A��!A���A���A�~�A�VA�+A��A�A��TA�ĜA���A�XA���A�|�A�K�A�&�A���A���A���A�z�A�`BA�O�A��A��A��A��A��yA��`A���A��jA��A���A�dZA�9XA�JA���A��hA�S�A�{A��A���A�p�A�VA�7LA�{A��#A���A��7A�|�A�t�A�jA�S�A�$�A�$�A�ZA�t�A�t�A�S�A�/A��A��A�r�A�A�9XA��A���A�n�A�G�A�+A��A�bA�1A���A��`A���A��FA���A�z�A�`BA�/A���A��jA���A�v�A�/A�%A��A��`A��RA�{A��-A�x�A�G�A�"�A�%A��A���A�I�A�~�A�/A��A���A��A�~�A�+A�ȴA���A��uA�l�A�I�A�"�A��A�A��A�9XA���A�dZA��yA���A�l�A�M�A�(�A��mA�(�A��wA� �A��9A�ƨA�Q�A�1A��yA��^A���A�x�A�t�A�x�A�^5A�G�A��A���A��hA�C�A��A��9A�`BA��A��\A�;dA���A���A�;dA��`A��PA��A���A�A�A�1A��;A���A�bNA�;dA��A��+A��A��HA���A���A��FA�jA�9XA��A���A�G�A%A~VA}ƨA}l�A{�TAz��Az��AzbNAz(�AzbAy�Ay��Ay�Ay�7Ayp�AyO�Ay&�Ax�Ax�uAw�mAwS�AwoAv�Av��AvȴAv�9Av�\Au�AtE�AsdZArr�Aq��Aq`BAp�yApjAo��Ao�AohsAn�`Anr�AnA�An �An{AnbAnAm�Am��AmAl��Al�\Alv�AlM�Al�Ak�Ak�;Ak�^Ak��Ak�Ak�Ak�Akt�Ak;dAj��Ajr�Aj{Ai��Ah�9Ag��Agl�Af��Af�Af�\Af^5Af=qAf(�Af{Ae�FAex�Ae�Ad��AdZAc�7Ab�uAa��Aa|�Aa7LAa
=A`�A`�jA`��A`jA`VA`9XA_�mA_��A_dZA^��A^��A^ZA^JA\�jA[�A[oAZVAY�mAY�7AY�AX�!AX��AX~�AX=qAW��AW�AW|�AWS�AW33AW/AW+AW�AWAV��AVĜAV�AV�\AV~�AVr�AVbNAVQ�AVI�AV9XAV(�AVbAU�mAUAU��AUt�AU\)AU?}AU�AT�ATĜAT �ASl�AR~�AQ+AP�uAPAO��AOK�AO
=AN��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            A�S�A�Q�A�Q�A�Q�A�S�A�Q�A�O�A�M�A�M�A�O�A�O�A�Q�A�Q�A�S�A�VA�XA�S�A�S�A�Q�A�Q�A�Q�A�S�A�VA�VA�VA�XA�XA�XA�XA�ZA�ZA�\)A�\)A�ZA�ZA�ZA�\)A�\)A�VA�?}A�
=A�VAӝ�A�\)A�  A��A�t�A�\)A��A�33AƏ\Aę�A�
=A�ZA�A�ffA�E�A�n�A�p�A���A��jA��A�z�A�1A�E�A��A��hA�ȴA�5?A��\A�7LA�n�A��A���A�|�A��-A�E�A�;dA�K�A�ȴA�^5A��A��\A��wA��wA���A�n�A|Q�Ay�Aw
=AqAnffAl�\Akl�AhA�Ae�^Aa��A`�A]
=AX�AW`BAV�9AV1'AUG�AQ��AN��AL  AIC�AD�AA�
A@(�A?S�A>�A> �A<�A:��A8�RA7�^A6��A5;dA4��A4ȴA4��A4�\A4�uA4�+A4n�A4I�A3��A3�A3+A2��A1��A0�`A0=qA0��A1%A/A.��A.JA.��A.��A-��A-�A-?}A,�A+ƨA+�7A*�A*z�A)�A)x�A(�HA'�;A'l�A%t�A$5?A$v�A$$�A#�FA#?}A"�`A"�!A"=qA!��A �A��A?}Ap�A�wA;dA1A�A�HAJA��A�A�HAjA�
AhsA33A��A�A�A?}A?}A��A(�A1A�^A��A�AXAA�DAr�A^5A-A�-A��A�A�A��A��Az�AVA�A��A+A�+AZA=qA�mA�A�yA�A�AVA�DAE�A�A$�AbA�#A|�AO�A%A
�`A
��A
��A
r�A
M�A
 �A	�TA	��A	C�A	%A��Az�A  A��A33A��AȴA��An�A(�A�-AhsA?}A"�A��AA�A�
Al�AoA��A�Ax�AVA ��A @��
@�33@�M�@��T@��@�?}@��@��@��D@� �@�|�@���@�{@���@��F@�;d@�ff@�V@�1'@�b@�C�@��@��@�7@�@�p�@���@�  @�ff@���@�x�@�G�@�&�@��@�u@�K�@��y@�=q@�@�7L@�9@�D@� �@��H@��T@�j@�t�@�o@�ȴ@�$�@�h@�x�@�hs@�j@� �@ߕ�@�S�@�o@�5?@���@�x�@��@�z�@�9X@�1@�  @��@�v�@�$�@�hs@ؼj@ؓu@�b@�\)@�$�@�X@ԓu@�I�@���@�C�@Ұ!@��@��@ѩ�@�x�@�7L@�V@���@Ь@�z�@�j@��@��
@ύP@�K�@�n�@�7L@�(�@�C�@���@�V@�-@��@ə�@���@��@ǝ�@�+@Ɨ�@�=q@ź^@�7L@���@�Q�@�\)@�@�{@��-@�O�@���@��j@�I�@� �@���@�+@�~�@��#@�V@�Z@�b@��w@�t�@�
=@��+@��@���@�&�@�bN@�A�@��;@�K�@��y@��!@�n�@�=q@�{@�@�x�@��@��@�j@��@��w@�l�@��@�-@��#@�x�@�G�@�7L@�V@��`@���@��@�  @�dZ@���@��H@��@��\@�J@���@�hs@�&�@��@���@��j@�A�@�b@���@�33@�"�@��y@���@���@�v�@�$�@���@���@�p�@�&�@��9@�j@��@��@��@�+@�ȴ@���@�@�7L@��@��D@���@�ƨ@���@��@��!@�E�@�5?@�J@��-@�x�@��@��9@��@��D@�Z@� �@��P@�"�@��@���@�ff@�$�@��T@��@�G�@�&�@���@���@��u@� �@��;@���@�33@�ȴ@���@��\@�$�@�`B@��/@�Z@��@�t�@�o@��H@���@�n�@��@��h@�G�@���@���@��@�r�@�1@��
@�S�@��@��@��!@�~�@�-@�{@��T@��@��@���@�r�@��;@���@�33@��y@��\@�E�@�J@���@���@�p�@�X@�G�@��@�Ĝ@��D@�9X@��w@�dZ@��@���@��y@��@���@�~�@�M�@�{@���@���@���@��7@�hs@�/@���@���@�z�@�I�@�b@��F@�\)@�C�@�;d@�+@�
=@���@��\@�E�@�{@��T@���@�p�@�G�@���@��@�r�@��@��@�ƨ@�\)@�33@�
=@��y@�ȴ@��!@��+@�V@���@�`B@��@��u@�1'@�b@��@|�@+@~��@~5?@}�T@}�h@|��@|�D@|j@|Z@|1@{�@{"�@z��@z��@y��@y��@yG�@y%@y%@x��@xĜ@xQ�@w�P@v�y@v�+@v$�@u�T@u�h@u�@tz�@t(�@s��@s�m@s�
@s�F@sC�@r�!@r~�@r^5@q��@qhs@q7L@p��@pr�@p  @o�P@o�@nȴ@n$�@m�h@mV@l(�@k��@k�@kdZ@kC�@j^5@i��@iX@i7L@i&�@h��@hr�@g\)@f�y@f�+@fff@f$�@e�h@e�@d��@dZ@c��@c@b��@bn�@aG�@`�u@`�@`bN@` �@_\)@^�R@^v�@^ff@]�@]p�@]`B@]�@\�/@\�j@\z�@[��@[dZ@[@Z�H@Z�\@Y�#@Yx�@Y7L@Y�@X��@X�`@X��@X�@XbN@W�@Wl�@W+@Vȴ@V�R@Vv�@V5?@V@U��@U�@U/@Tj@S�F@SC�@So@R�H@R~�@RJ@Q��@Qhs@Q&�@Pr�@P �@O�P@O
=@N��@N@M?}@L�/@L��@L(�@K�F@KS�@K33@K@J��@J~�@J�@I�7@H�`@H�u@HbN@G��@G;d@F�y@F�+@F@E�@D�j@DI�@D(�@C�@C@B��@Bn�@B=q@A�#@A�7@A�@@Ĝ@@��@@1'@?�w@>�R@>��@>�+@>V@>$�@=�-@=p�@=?}@<��@<�@;�
@;t�@;C�@;@:�!@:�\@:=q@9�@9�@9�#@9��@9��@9hs@97L@8�`@8��@8bN@81'@7�;@7�@7��@7l�@6�y@6��@6v�@65?@6@5��@5�@4��@4��@4j@4Z@4�@3��@3dZ@3"�@2�H@2��@2-@1�#@17L@0��@0A�@/�;@/��@/|�@/�@.�R@.�+@.$�@-��@-��@-p�@-�@,�/@,�@,z�@,I�@,1@+��@+t�@+S�@+"�@*�@*�H@*��@*�\@*n�@*M�@*-@)��@)�^@)hs@)%@(��@(�u@(bN@( �@'�@'|�@';d@'�@&��@&ȴ@&ff@%�@%�@%`B@%O�@%O�@%V@$�@$�j@$z�@$1@#�@#dZ@#33@#@"�H@"��@"-@!��@!��@!X@!�@!%@ ��@ ��@ r�@  �@ b@�@��@K�@+@
=@ȴ@��@�+@v�@ff@ff@5?@{@��@��@�h@�h@�h@�@`B@/@��@�j@�D@�@�F@��@dZ@S�@C�@C�@C�@33@�@�H@�!@n�@M�@-@�@�@��@�#@�^@x�@hs@hs@X@X@�@Ĝ@��@�u@ �@�@��@�@l�@K�@+@+@�@
=@
=@�R@��@��@�+@�+@ff@5?@$�@�T@��@�@p�@p�@p�@O�@��@�/@z�@I�@9X@�@�m@�F@�@33@"�@�@��@�!@�\@n�@=q@��@��@hs@x�@hs@7L@&�@��@�9@��@�u@�@r�@�A�Q�A�VA�O�A�VA�S�A�O�A�S�A�S�A�M�A�O�A�S�A�S�A�O�A�Q�A�VA�Q�A�Q�A�VA�S�A�O�A�S�A�O�A�M�A�M�A�Q�A�K�A�K�A�Q�A�O�A�K�A�M�A�S�A�M�A�K�A�O�A�Q�A�M�A�M�A�S�A�Q�A�O�A�S�A�S�A�O�A�Q�A�Q�A�M�A�O�A�XA�VA�Q�A�VA�XA�S�A�S�A�XA�VA�S�A�XA�XA�S�A�VA�ZA�XA�VA�ZA�XA�S�A�VA�ZA�S�A�VA�XA�O�A�O�A�VA�S�A�O�A�S�A�VA�O�A�S�A�VA�Q�A�O�A�VA�S�A�O�A�Q�A�Q�A�M�A�O�A�S�A�Q�A�O�A�S�A�S�A�O�A�O�A�VA�S�A�O�A�S�A�VA�Q�A�Q�A�VA�XA�S�A�S�A�XA�VA�VA�XA�VA�Q�A�XA�VA�S�A�XA�XA�S�A�S�A�XA�XA�S�A�VA�ZA�S�A�VA�ZA�ZA�S�A�VA�ZA�XA�VA�XA�ZA�XA�S�A�ZA�ZA�VA�VA�ZA�ZA�VA�XA�ZA�VA�VA�ZA�XA�VA�ZA�\)A�XA�XA�\)A�ZA�VA�XA�\)A�\)A�XA�ZA�^5A�XA�ZA�\)A�^5A�ZA�ZA�^5A�\)A�ZA�ZA�^5A�^5A�ZA�\)A�^5A�ZA�ZA�^5A�\)A�ZA�\)A�\)A�VA�VA�ZA�XA�XA�^5A�\)A�XA�ZA�^5A�^5A�ZA�XA�^5A�^5A�ZA�ZA�`BA�^5A�\)A�ZA�ZA�VA�M�A�I�A�M�A�G�A�C�A�9XA�33A�1'A��A�VA�1A�%A�A��A�ƨAԋDA�K�A�JA���A��;A���A�ȴA�AӋDA�&�A�  A��/A�z�A�;dA�AхA�z�A�hsA���AмjA�x�A�A�A�33A�1A���Aϝ�AΡ�Aͥ�A�`BA�=qA��A��
A�ƨA̩�ÃA���A�?}AʋDA�1'A��#A���Aɺ^AɬAɣ�A�z�A�%A�^5A��
A�5?Aƺ^A�33AŇ+A�bA��
Aę�A�l�A�VA�A�A�-A��A�1A��AþwAÇ+A�I�A�K�A�K�A�5?A�&�A�{A���A��A���A®A�x�A�XA�?}A�7LA��A�ffA�$�A��A��A�{A��A��wA��PA�\)A���A���A�;dA��yA��A��A��A��A���A�oA���A�VA�+A���A�l�A�-A� �A�bA���A��A���A��wA��A���A��PA��A�x�A�O�A�"�A�{A�%A��A���A��A���A��7A��+A�t�A�l�A�bNA�M�A�=qA�-A�$�A��A���A��TA���A���A�r�A�E�A�7LA��A���A��yA��#A�ȴA��+A�Q�A�=qA� �A���A�bNA�A��-A�ffA���A��PA�v�A�O�A��A�  A���A�~�A�M�A�+A�oA�A���A��A��`A��/A��A���A��^A���A��A�^5A�33A��A�1A��A��#A��^A���A��A�p�A�S�A�C�A�1'A� �A�A��A��;A���A�A��FA���A��+A�r�A�I�A�-A�1A���A���A�VA�$�A���A���A�|�A�5?A�bA�A���A��TA��
A�ƨA��jA�Q�A��
A�hsA���A�jA�hsA� �A��TA��jA���A�x�A�p�A�C�A�+A���A��A��/A���A���A�ȴA���A��jA��RA��!A���A��PA�^5A�7LA��A�(�A��A�"�A�"�A���A��A���A��!A���A���A�~�A�VA�+A��A�A��TA�ĜA���A�XA���A�|�A�K�A�&�A���A���A���A�z�A�`BA�O�A��A��A��A��A��yA��`A���A��jA��A���A�dZA�9XA�JA���A��hA�S�A�{A��A���A�p�A�VA�7LA�{A��#A���A��7A�|�A�t�A�jA�S�A�$�A�$�A�ZA�t�A�t�A�S�A�/A��A��A�r�A�A�9XA��A���A�n�A�G�A�+A��A�bA�1A���A��`A���A��FA���A�z�A�`BA�/A���A��jA���A�v�A�/A�%A��A��`A��RA�{A��-A�x�A�G�A�"�A�%A��A���A�I�A�~�A�/A��A���A��A�~�A�+A�ȴA���A��uA�l�A�I�A�"�A��A�A��A�9XA���A�dZA��yA���A�l�A�M�A�(�A��mA�(�A��wA� �A��9A�ƨA�Q�A�1A��yA��^A���A�x�A�t�A�x�A�^5A�G�A��A���A��hA�C�A��A��9A�`BA��A��\A�;dA���A���A�;dA��`A��PA��A���A�A�A�1A��;A���A�bNA�;dA��A��+A��A��HA���A���A��FA�jA�9XA��A���A�G�A%A~VA}ƨA}l�A{�TAz��Az��AzbNAz(�AzbAy�Ay��Ay�Ay�7Ayp�AyO�Ay&�Ax�Ax�uAw�mAwS�AwoAv�Av��AvȴAv�9Av�\Au�AtE�AsdZArr�Aq��Aq`BAp�yApjAo��Ao�AohsAn�`Anr�AnA�An �An{AnbAnAm�Am��AmAl��Al�\Alv�AlM�Al�Ak�Ak�;Ak�^Ak��Ak�Ak�Ak�Akt�Ak;dAj��Ajr�Aj{Ai��Ah�9Ag��Agl�Af��Af�Af�\Af^5Af=qAf(�Af{Ae�FAex�Ae�Ad��AdZAc�7Ab�uAa��Aa|�Aa7LAa
=A`�A`�jA`��A`jA`VA`9XA_�mA_��A_dZA^��A^��A^ZA^JA\�jA[�A[oAZVAY�mAY�7AY�AX�!AX��AX~�AX=qAW��AW�AW|�AWS�AW33AW/AW+AW�AWAV��AVĜAV�AV�\AV~�AVr�AVbNAVQ�AVI�AV9XAV(�AVbAU�mAUAU��AUt�AU\)AU?}AU�AT�ATĜAT �ASl�AR~�AQ+AP�uAPAO��AOK�AO
=AN��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
]/B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]/B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]/B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\]B
[�B
[�B
[#B
ZQB
ZQB
aB
b�B
s�B
��B
��B
��B
��B
��B
�^B
��B
��B
��B
�B
�B
��BB!-B>BB5�BW�B��B�=B��B��B�B�	B��B��B�GBw2B\]BLdB:�B'�BA B.B
�"B
�B
ѷB
��B
��B
GB
xB	�JB	�,B	��B	��B	��B	��B	�bB	� B	}"B	y	B	~�B	u�B	{JB	{JB	�xB	�B	��B	�kB	��B	�7B	�RB	��B	��B	��B	��B	� B	|�B	z�B	zDB	�B	��B	��B	�bB	��B	��B	�+B	�B	�3B	�KB	��B	��B	�	B	�(B
�B
B
�B
�B
MB
�B
�B
,�B
HB
\�B
Q�B
F?B
@�B
H�B
W�B
Z�B
f2B
jKB
m]B
j�B
h�B
dZB
a�B
^�B
`vB
_pB
Z�B
Y�B
R�B
IRB
XEB
_B
]/B
]�B
\�B
[�B
Z�B
WsB
S�B
Q�B
L�B
OvB
UgB
Z�B
WsB
T�B
T�B
VB
Q�B
OBB
NpB
N�B
OB
OB
O�B
N�B
K�B
H�B
J#B
L�B
NB
K�B
RTB
S�B
R�B
RTB
R�B
T�B
XyB
YKB
Z�B
\]B
[�B
[#B
\�B
_pB
]�B
]/B
\�B
]dB
]�B
^B
[�B
[�B
ZQB
Z�B
ZQB
Y�B
WsB
VB
Q�B
Q�B
P�B
OvB
O�B
R�B
T�B
VmB
V�B
VB
U�B
T�B
T,B
T,B
S&B
R�B
RTB
QNB
P�B
P�B
OvB
N�B
M�B
LdB
J�B
JXB
HKB
G�B
F�B
F�B
E�B
E�B
C-B
B[B
A�B
B[B
@B
?}B
<�B
:*B
8RB
6�B
6�B
5B
4nB
2�B
0�B
3hB
1'B
/�B
.�B
.}B
.�B
0�B
0�B
0UB
1[B
/�B
/�B
-B
(�B
'�B
'�B
'RB
$tB
#�B
$B
#�B
"4B
"4B
!�B
!�B
%B
'�B
'B
#nB
#:B
"hB
"�B
"4B
#�B
!�B
VB
 �B
�B
B
IB
�B
qB
CB
=B
�B
�B
�B
$B
�B
�B
B
{B
�B
{B
�B
B
�B
�B
4B
hB
B
hB
B
�B
�B
�B
{B
{B
�B
{B
�B
�B
�B
�B
B
�B
�B
�B
�B
FB
@B
oB
�B
hB
hB
�B
 B
4B
bB
.B
�B
�B
�B
�B
.B
�B
�B
�B
�B
 B
�B
�B
�B
�B
�B
.B
�B
.B
�B
�B
�B
�B
hB
�B
�B
�B
�B
B
B
�B
�B
B
B
B
B
FB
�B
�B
MB
MB
MB
B
MB
B
MB
�B
�B
�B
B
�B
B
MB
MB
MB
�B
�B
�B
B
SB
B
�B
�B
�B
�B
_B
+B
1B
�B
�B
eB
eB
eB
1B
qB
=B
CB
xB
�B
�B
�B
�B
�B
�B
�B
IB
~B
�B
�B
�B
�B
B
OB
B
~B
�B
B
OB
�B
 �B
�B
 �B
 �B
!�B
!-B
!bB
"hB
"�B
"hB
%zB
%zB
%B
%�B
%FB
$�B
$B
$�B
$tB
$�B
$tB
$�B
%zB
%zB
&�B
'�B
(�B
)�B
)_B
*�B
+�B
,=B
+�B
,B
,qB
,�B
,�B
-CB
-CB
-CB
-CB
-CB
-�B
.B
-�B
.B
.�B
.�B
.}B
.IB
.�B
/�B
0!B
/�B
/�B
0UB
1'B
1'B
1'B
0�B
1�B
1�B
1�B
2-B
2aB
2-B
2�B
2�B
2�B
3�B
3hB
3�B
3hB
3�B
4B
3�B
3�B
4nB
4nB
49B
5?B
6B
5�B
6zB
6zB
7LB
7LB
7�B
7�B
7�B
7�B
7�B
7�B
8RB
8�B
8�B
9XB
:*B
:^B
:�B
:�B
:�B
:�B
:�B
;0B
;dB
;�B
<B
<B
<B
<B
<6B
<jB
=B
<�B
=<B
=qB
=�B
>wB
>�B
>�B
>�B
?B
>�B
?}B
?�B
@OB
@B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B[B
A�B
B[B
B�B
B�B
B�B
B�B
C-B
B�B
CaB
C�B
D�B
C�B
D�B
E�B
FtB
FB
F�B
F�B
F�B
G�B
G�B
HB
H�B
IRB
IRB
IB
IRB
I�B
I�B
I�B
J#B
J#B
J�B
J�B
K�B
K^B
K^B
K)B
K�B
K�B
LdB
MB
L�B
MB
L�B
MjB
M�B
N<B
NB
N<B
NB
NB
N<B
N�B
OBB
N�B
N�B
OvB
O�B
OvB
O�B
O�B
PHB
P}B
P�B
P�B
Q�B
Q�B
Q�B
S�B
S&B
R�B
R�B
R�B
T,B
T,B
T,B
S�B
S�B
S�B
T,B
U�B
U�B
U�B
U�B
U�B
VmB
V�B
V�B
V�B
V�B
W�B
W?B
WsB
YKB
X�B
XEB
XEB
XEB
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
\)B
[�B
\]B
\�B
\�B
]/B
\�B
]/B
]/B
]dB
]dB
]dB
]�B
^5B
^�B
^�B
^jB
^�B
^�B
^�B
_B
_B
_;B
`B
`BB
`�B
`vB
`vB
`�B
aB
aB
aHB
aB
bNB
a�B
bNB
b�B
b�B
cTB
d&B
c�B
c�B
dZB
d�B
d�B
d�B
d�B
d�B
d�B
e`B
e�B
e�B
e�B
e�B
f2B
ffB
e�B
f2B
e�B
e�B
gB
f�B
f�B
g�B
g�B
g�B
h>B
h>B
hsB
h�B
iDB
iDB
iDB
i�B
jB
j�B
jB
jB
jB
jB
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
l"B
lWB
lWB
l�B
l�B
l�B
l�B
l�B
m)B
m)B
m)B
m�B
m�B
m�B
m�B
ncB
ncB
n/B
ncB
o B
o5B
o5B
oiB
o�B
o�B
o�B
p�B
p�B
p�B
p�B
qB
q�B
q�B
q�B
q�B
rB
r|B
r�B
sMB
s�B
s�B
tB
s�B
s�B
tB
tTB
t�B
t�B
uZB
uZB
u�B
u�B
uZB
u�B
u�B
u�B
u�B
v`B
v�B
v�B
v�B
v�B
v�B
w2B
w2B
w2B
w2B
wfB
w�B
w�B
xlB
xlB
x�B
x�B
x�B
y>B
y�B
zB
zDB
z�B
z�B
zxB
{B
{JB
{�B
{�B
{�B
|B
|PB
|B
|B
|�B
}VB
}�B
}VB
}�B
}�B
}�B
}�B
~]B
~�B
~�B
.B
.B
.B
�4B
�iB
�4B
�iB
�4B
�iB
��B
��B
��B
�B
�;B
�oB
�oB
�oB
�oB
�oB
�B
��B
�uB
��B
��B
��B
��B
��B
�B
��B
�B
��B
��B
�MB
��B
��B
�B
�B
�SB
�B
�B
�SB
��B
��B
��B
��B
��B
�%B
�%B
�%B
��B
�YB
��B
��B
�+B
�+B
�_B
��B
��B
��B
��B
��B
�fB
��B
�1B
��B
��B
��B
�B
�B
�7B
�7B
�7B
��B
��B
��B
��B
��B
�	B
�=B
�=B
�rB
��B
��B
��B
��B
��B
�B
�xB
�DB
�B
�JB
�B
�~B
�~B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�"B
��B
��B
��B
�(B
�\B
��B
��B
��B
��B
��B
��B
��B
��B
��B
^jB
\)B
^�B
\)B
\]B
]�B
\�B
[�B
]�B
]/B
[�B
\�B
]/B
\]B
[�B
\�B
]�B
[�B
\�B
]�B
[�B
]/B
\�B
\�B
[WB
]dB
]dB
\]B
\)B
]/B
\�B
[WB
\�B
]�B
\�B
[�B
^5B
\�B
[�B
\�B
]�B
\)B
\)B
]�B
]/B
[�B
\�B
]�B
[�B
[�B
]�B
\]B
[�B
]�B
]�B
\]B
]dB
^B
\�B
\)B
^B
]dB
[�B
]/B
^B
\]B
\�B
^5B
]dB
[�B
]�B
\�B
\�B
]/B
]�B
[�B
\]B
^B
\]B
[�B
]�B
\�B
[�B
\�B
^B
[�B
\�B
]�B
\]B
\]B
]�B
\�B
[�B
\�B
]�B
\]B
[�B
]�B
]dB
[�B
\�B
^5B
]/B
[�B
]�B
]�B
\]B
[�B
]�B
]�B
[�B
\�B
]/B
\]B
\]B
^B
[�B
\�B
^B
\�B
\]B
]�B
]�B
[�B
\�B
^B
\�B
[�B
^B
\�B
[�B
[�B
]�B
]�B
[�B
\]B
^B
]/B
\)B
\�B
^B
[�B
\)B
^B
]�B
\�B
\�B
^5B
\�B
\�B
]�B
]/B
[�B
]/B
]�B
\]B
\)B
]�B
]�B
\)B
\�B
]�B
]dB
[�B
\�B
^B
]/B
\)B
]�B
]�B
\]B
[�B
]dB
]dB
[�B
\�B
^B
]dB
\]B
\]B
^5B
\�B
[�B
\�B
]/B
[WB
\�B
]�B
\]B
\)B
]�B
]/B
[�B
\]B
\�B
[�B
[�B
\�B
\�B
[#B
[WB
\)B
\�B
[�B
Z�B
\�B
[�B
YB
Z�B
\]B
[�B
Y�B
ZB
\]B
[WB
YB
Y�B
YB
\]B
ZQB
YB
_;B
[�B
YB
X�B
X�B
[�B
_;B
]/B
c�B
b�B
`B
`B
_B
^5B
]�B
ffB
k�B
f�B
l"B
q�B
v`B
y>B
��B
zB
}VB
��B
��B
�eB
�B
��B
��B
��B
��B
��B
��B
�hB
��B
�B
��B
�3B
��B
��B
ӏB
�zB
�&B
�B
��B
��B
��B
��B
�B
�B
�^B
�qB
��B
��B
�B
��B
�<B
�)B
�^B
уB
��B
��B
�9B
��B
��B
��B
��B
�QB
��B
�DB
�B
�5B
�oB
�B
�GB
��B
�B
�+B
��B
��B
�.B
��B
��BhB�B�B�B�B�BBBB$�B+6B5�B7�B;0B;�B8�BO�BT,BPB7�B5B-�B*�B.B1'B0�B+B,�B/B/�B2�B6FB7�B8�B9XB8�B8�B?B:�B9$B9�B;dB<jB>�B@BF?BC�BC�B@�B?HB@�BB[BF�BHKBIRBN�BPBR B^jBf�Bh
Bi�Bo BqABqBqBoiB{JB|�Bv`Bw�B��B�B��B�B�{B��B�FB�VB��B��B�{B�1B��B��B��B�+B�_B��B��B��B��B��B�_B��B�kB�CB��B�VB�CB�IB�B�!B�!B�bB��B��B�-B�OB��B��B��B�!B�B��B�-B��B��B�\B��B�B��B��B�B��B�kB��B��B��B�*B��B�nB�-B��B��B��B�VB�IB��B�-B��B��B��B��B��B�eB��B��B�kB�_B�-B��B��B�@B��B��B��B��B�B��B��B��B��B�4B��B�xB�+B��B��B�B�GB��B�SB��B��B��B�B�{B��B�AB{B|�B|PB{�By�BzxB�oBsMBhsBgBe�BcTBffB]�BXBV�B_�BOBBOvBNpBN�BLdBQ�BL�BJ�BK^BK^BHKBE�BH�BA�B@�B?B9�B6B5�B/B0UB+�B33B+B)�B'RB%zB%�B'�B(�BB&�B;�B=<B=�B=B;�BF?BNpBd�B2�B�B�BFBoB�B
�B	B	lB�B�BMB�B �B
��B
�JB
��B
�B
�xB
��B
��B
�5B
�B
�WB
�B
��BYB
��B
�NB
�;B
یB
�KB
�B
��B
��B
�iB
ŢB
ĜB
�UB
�jB
��B
�$B
�?B
�B
�B
��B
�FB
��B
��B
��B
��B
�B
�YB
�B
�{B
�GB
w2B
u%B
ncB
xB
�AB
��B
a�B
\)B
jKB
FB
<�B
8�B
5�B
.B
&LB
$�B
#B
+�B
 �B
"hB
�B
�B
"�B
�B
;B
oB
"B
 �B	��B	�2B	�B	�B	��B	��B	�B	�DB	�B	�sB	��B	��B	�jB	ŢB	�?B	ʌB	��B	�B	��B	�B	�6B	ĜB	��B	�mB	�tB	�aB	�B	�jB	��B	�B	��B	�'B	�B	�*B	��B	�B	��B	��B	��B	�4B	��B	�'B	��B	��B	�@B	�RB	��B	�kB	�B	��B	��B	�:B	��B	��B	�B	�+B	�(B	�B	�B	��B	��B	��B	�B	�GB	�B	�B	cB	|PB	|�B	z�B	z�B	{�B	��B	��B	}VB	{B	zB	{B	z�B	zxB	wfB	{JB	y�B	wfB	v�B	uZB	u�B	|�B	|�B	��B	zDB	�{B	�1B	�SB	|�B	z�B	xlB	t�B	w�B	sB	qvB	s�B	y�B	v�B	uZB	y�B	{�B	|�B	��B	�B	xlB	w2B	v+B	u�B	s�B	w2B	w�B	w�B	{JB	�B	�B	��B	��B	�B	~�B	�SB	�VB	��B	�SB	��B	�(B	�B	�{B	�B	��B	�bB	�@B	��B	�B	�B	��B	�B	��B	�+B	�+B	�CB	��B	�qB	�B	��B	�1B	�_B	�_B	��B	��B	�7B	��B	��B	��B	��B	��B	��B	�	B	��B	�1B	��B	�B	��B	��B	�OB	��B	�4B	�RB	��B	�@B	��B	�:G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            B
V�B
VxB
VxB
VxB
VxB
VxB
VDB
VDB
VDB
VDB
VDB
VxB
VxB
VxB
V�B
V�B
VxB
VDB
VxB
VxB
V�B
VxB
VxB
V�B
V�B
V�B
VxB
V�B
V�B
V�B
V�B
VxB
V�B
V�B
VxB
VB
U�B
U>B
T�B
TB
TB
Z�B
\iB
m4B
�LB
�EB
�QB
�WB
��B
�B
�WB
ʗB
�~B
�B
�bB
�=B�B�B7�B/ZBQZB��B��B�BB�3B��B��B�OB��B|�Bp�BVBFB4EB!�B:�B	�B
��B
��B
�iB
�EB
�tB
@�B
*B	��B	��B	�5B	�mB	��B	��B	�B	y�B	v�B	r�B	x�B	o@B	t�B	t�B	�*B	��B	�9B	�B	��B	��B	�B	�gB	��B	�dB	��B	y�B	vkB	t�B	s�B	z�B	�nB	��B	�B	��B	�OB	��B	��B	��B	��B	�{B	�@B	�B	��B
�B
�B
9B
?B
�B
�B
�B
&WB
A�B
VDB
K5B
?�B
:5B
B�B
Q�B
TlB
_�B
c�B
gB
deB
b�B
^B
[�B
XPB
Z(B
Y"B
T8B
S�B
L�B
CB
Q�B
X�B
V�B
W~B
VxB
U�B
TlB
Q%B
MuB
K�B
FB
I(B
OB
T�B
Q%B
N�B
N�B
O�B
K5B
H�B
H"B
HWB
H�B
H�B
I]B
H�B
EyB
B2B
C�B
FJB
G�B
EDB
LB
MAB
L;B
LB
L�B
N|B
R+B
R�B
T8B
VB
U>B
T�B
VDB
Y"B
W~B
V�B
VDB
WB
WJB
W�B
U�B
U>B
TB
T8B
TB
S�B
Q%B
O�B
K�B
K5B
J�B
I(B
I�B
L�B
NGB
PB
P�B
O�B
OMB
NGB
M�B
M�B
L�B
L;B
LB
K B
J�B
J�B
I(B
HWB
G�B
FB
D�B
D
B
A�B
A�B
@�B
@�B
?TB
?TB
<�B
<B
;�B
<B
9�B
9/B
6QB
3�B
2B
0�B
0�B
.�B
. B
,�B
*pB
-B
*�B
)5B
(dB
(/B
(�B
*pB
*<B
*B
+B
)5B
)5B
&�B
"?B
!9B
!mB
!B
&B
UB
�B
UB
�B
�B
�B
}B
�B
!9B
 �B
 B
�B
B
�B
�B
UB
�B
B
�B
6B
�B
�B
XB
#B
�B
�B
�B
EB
�B
�B
?B
�B
�B
-B
�B
-B
�B
�B
�B
UB

�B
B
�B
B
�B
�B
UB
3B
-B
-B
�B
-B
aB
�B
3B
�B
�B
�B
�B
aB
3B
�B
�B
!B
�B
B
B

}B

�B

�B

B
	�B
	�B
	�B
	wB
qB
	�B
�B
OB
�B
OB

�B

}B

IB

IB

}B
OB
	�B

}B
	�B
	wB

IB
	�B
	wB
B
UB
�B
UB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
gB
3B
�B
�B
�B
�B
�B
�B
�B
9B
3B
�B
�B
�B
�B
�B
�B
�B
3B
�B
gB
�B
B
�B
9B
nB
9B
?B
B
�B
�B
�B
�B
B
B
B
�B
#B
�B
�B
*B
�B
^B
�B
�B
^B
�B
�B
�B
0B
�B
^B
�B
�B
�B
B
�B
0B
dB
�B
B
�B
wB
�B
�B
wB
HB
�B
B
B
�B
B
,B
,B
�B
�B
�B
[B
�B
�B
&B
[B
&B
[B
,B
,B
 3B
!9B
"?B
#�B
#B
$�B
%QB
%�B
%�B
%�B
&#B
&WB
&�B
&�B
&�B
&�B
&�B
&�B
'^B
'�B
'�B
'�B
(dB
(�B
(/B
'�B
(�B
)5B
)�B
)5B
)�B
*B
*�B
*�B
*�B
*pB
+BB
+�B
+�B
+�B
,B
+�B
,|B
,�B
,HB
-�B
-B
-�B
-B
-NB
-�B
-NB
-�B
. B
. B
-�B
.�B
/�B
/ZB
0,B
0,B
0�B
0�B
12B
12B
1�B
1�B
1�B
1�B
2B
2mB
2mB
3
B
3�B
4B
4yB
4�B
4yB
4yB
4yB
4�B
5B
5B
5�B
5�B
5�B
5�B
5�B
6B
6�B
6�B
6�B
7#B
7�B
8)B
8�B
8�B
8]B
8�B
8�B
9/B
9cB
:B
9�B
:5B
:�B
:5B
:jB
;;B
;;B
;�B
<B
;�B
<B
<�B
<vB
<�B
<�B
<�B
<�B
=B
=HB
>NB
=�B
>�B
?TB
@&B
?�B
@ZB
@ZB
@�B
A`B
A�B
A�B
BfB
CB
CB
B�B
CB
ClB
C8B
C�B
C�B
C�B
D�B
DsB
EDB
EB
EB
D�B
EDB
EDB
FB
F�B
FB
F�B
FB
GB
GQB
G�B
G�B
G�B
G�B
G�B
G�B
HWB
H�B
H�B
H�B
I(B
I]B
I(B
I�B
I�B
I�B
J/B
J�B
JcB
KiB
K5B
K�B
MAB
L�B
L�B
LoB
LoB
M�B
M�B
M�B
M�B
MuB
M�B
M�B
O�B
OMB
O�B
OMB
O�B
PB
PSB
PSB
P�B
P�B
QZB
P�B
Q%B
R�B
R`B
Q�B
Q�B
Q�B
S�B
S�B
S�B
SfB
TlB
TlB
T8B
T�B
T�B
T�B
T�B
UrB
UrB
U�B
UrB
VB
V�B
VxB
V�B
V�B
V�B
V�B
WB
WB
WB
W~B
W�B
XPB
X�B
XB
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Z\B
Z(B
Z(B
Z�B
Z�B
Z�B
Z�B
Z�B
\ B
[cB
\ B
\4B
\4B
]B
]�B
]oB
]�B
^B
^uB
^AB
^AB
^uB
^�B
^�B
_B
_{B
_�B
_GB
_�B
_�B
`B
_{B
_�B
_�B
_�B
`�B
`MB
`�B
a�B
aSB
a�B
a�B
a�B
b%B
bYB
b�B
b�B
b�B
c_B
c�B
deB
d1B
d1B
d1B
d1B
d�B
d�B
deB
e7B
e7B
elB
e�B
e�B
e�B
f	B
f	B
frB
f�B
f�B
frB
f�B
f�B
f�B
f�B
gCB
gxB
g�B
g�B
hB
hB
g�B
hB
h�B
h�B
h�B
iB
iPB
i�B
i�B
jVB
j�B
j�B
j�B
j�B
k\B
k\B
k�B
k�B
k�B
l.B
lbB
l�B
m4B
mhB
m�B
m�B
m�B
m�B
nB
n:B
n�B
oB
oB
o@B
ouB
oB
ouB
ouB
ouB
o�B
pB
pFB
pFB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
qLB
q�B
rB
rB
rSB
rSB
r�B
r�B
s�B
s�B
s�B
t_B
t_B
t*B
t�B
t�B
ueB
u�B
ueB
u�B
vB
u�B
u�B
vkB
wB
w=B
wB
wqB
wqB
w=B
w�B
xB
xCB
x�B
x�B
x�B
x�B
y�B
zB
y�B
zB
y�B
zB
zOB
z�B
z�B
z�B
z�B
{!B
{!B
{!B
{!B
{!B
{�B
{�B
|'B
|\B
|\B
|�B
|\B
|�B
|�B
|�B
|�B
}bB
}�B
}�B
~hB
~�B
~�B
~�B
B
~�B
~�B
B
:B
:B
nB
nB
�B
�B
�B
�B
�@B
�B
�@B
��B
��B
��B
�B
�FB
�zB
��B
��B
�zB
�B
��B
��B
�LB
��B
��B
��B
��B
��B
��B
��B
�RB
�RB
�RB
��B
��B
��B
��B
��B
�$B
�XB
��B
��B
��B
��B
��B
�*B
��B
��B
��B
��B
�0B
�0B
��B
��B
�6B
�6B
�kB
��B
�kB
��B
��B
��B
�<B
�<B
�qB
��B
�B
�CB
�wB
��B
�wB
�wB
�CB
�wB
�wB
�qB
XB
U�B
XPB
U�B
VB
W~B
VDB
UrB
W~B
V�B
U>B
V�B
V�B
VB
UrB
VDB
W~B
U>B
VDB
W~B
U�B
V�B
V�B
V�B
U	B
WB
WB
VB
U�B
V�B
VDB
U	B
VDB
WJB
VDB
U>B
W�B
VxB
U�B
VxB
W~B
U�B
U�B
W~B
V�B
UrB
V�B
W~B
UrB
U�B
W~B
VB
U�B
WJB
WJB
VB
WB
W�B
VDB
U�B
W�B
WB
UrB
V�B
W�B
VB
VxB
W�B
WB
UrB
WJB
V�B
VxB
V�B
WJB
U�B
VB
W�B
VB
U>B
WJB
VDB
U>B
V�B
W�B
UrB
VxB
WJB
VB
VB
WJB
V�B
U�B
VxB
W~B
VB
U�B
WJB
WB
UrB
VxB
W�B
V�B
U�B
W~B
WJB
VB
U�B
WJB
WJB
U�B
V�B
V�B
VB
VB
W�B
U�B
VxB
W�B
VDB
VB
WJB
W~B
U�B
VDB
W�B
VxB
U�B
W�B
V�B
U�B
U�B
W~B
W~B
U�B
VB
W�B
V�B
U�B
VxB
W�B
U�B
U�B
W�B
WJB
VDB
VDB
W�B
VxB
VDB
W~B
V�B
U�B
V�B
W~B
VB
U�B
WJB
WJB
U�B
VxB
WJB
WB
UrB
VxB
W�B
V�B
U�B
W~B
WJB
VB
U�B
WB
WB
U�B
VDB
W�B
WB
VB
VB
W�B
VDB
U�B
V�B
V�B
U	B
VDB
WJB
VB
U�B
W~B
V�B
UrB
VB
VDB
UrB
U�B
VxB
VxB
T�B
U	B
U�B
V�B
UrB
T�B
VDB
U�B
S1B
T�B
VB
U>B
SfB
S�B
VB
U	B
S1B
SfB
S1B
VB
TB
R�B
X�B
U>B
R�B
R`B
R�B
UrB
X�B
V�B
]oB
\4B
Y�B
Y�B
X�B
W�B
W~B
`B
elB
`�B
e�B
k\B
pB
r�B
~3B
s�B
wB
�kB
�^B
�B
��B
�CB
�tB
�tB
��B
ɑB
��B
�B
��B
��B
�vB
��B
�`B
�mB
�AB
�,B
��B
ƳB
�pB
��B
�>B
�sB
��B
��B
�B
�#B
��B
��B
��B
�5B
��B
��B
�B
�5B
˞B
˞B
��B
ҔB
ԠB
�xB
ۗB
�B
�~B
��B
�7B
��B
�!B
�VB
��B
�B
��B
��B
�eB
�qB
��B
�CB
�kBB�B�BOB
}B	wB�B�B�B[B$�B/�B12B4�B5B2�BI�BM�BI�B12B.�B'�B$KB'�B*�B*<B$�B&�B(�B)5B,�B/�B1�B2mB3
B28B28B8�B4yB2�B3�B5B6B8]B9�B?�B=�B=�B:�B8�B:5B<B@�BA�BCBH�BI�BK�BXB`�Ba�Bc_Bh�Bj�Bj�Bj�BiBt�Bv�BpBqLB|\B}�B�FB��B�-B�}B��B�B��B��B�-B��B�XB�RB�?B��B�B��B��B�EB��B�9B�B�zB�B��B�pB�B��B��B��B��B��B�B��B�pB��B�B�BB��B�}B��B��B��B��B�pB�pB�B�wB��B�HB�aB��B�3B�B��B�dB��B��B�QB� B��B�BB��B��B�B��B��B��B��B�^B�<B�dB�jB�B�aB�zB�B�B��B��B��B��B�gB��B��B�UB��B��B��B�OB��B��B�EB�*B��B~hB�@B~�B|�B�^BB��B}�B|�B|�B}-B|�B{�Bt�Bv�BvBu�Bs�Bt*B{!Bl�Bb%B`�B_GB]B`BW~BQ�BP�BYVBH�BI(BH"BH�BFBK5BFBD�BEBEBA�B?TBBfB;pB:jB8�B3>B/�B/�B(�B*B%�B,�B$�B#yB!B,B�B!mB"sB�B �B5B6�B7�B6�B5KB?�BH"B^uB,|B�BXB�B!B<B�B�BB �B
�:B
��B
��B
��B
��B
��B
��B
�1B
�*B
�~B
�uB
��B
�1B
�	B
��B
�B B
ܝB
� B
��B
�>B
��B
��B
ӚB
�xB
�B
�TB
�NB
�B
�B
؅B
��B
��B
��B
��B
��B
��B
�}B
�[B
�dB
�BB
��B
�B
��B
}-B
|�B
p�B
n�B
hB
q�B
{�B
zOB
[cB
U�B
c�B
?�B
6QB
2mB
/�B
'�B
�B
[B
�B
%�B
�B
B
^B
kB
�B
�B	��B
!B
�B	�OB	�wB	��B	��B	�:B	�B	�rB	�JB	��B	�cB	�%B	ͪB	ΰB	�B	�TB	��B	�>B	��B	��B	�sB	��B	��B	�NB	�ZB	�B	�&B	�B	϶B	�B	�>B	��B	B	��B	��B	��B	�?B	��B	��B	�UB	�NB	��B	�<B	��B	��B	��B	��B	�B	�jB	�B	��B	��B	�OB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	~hB	~�B	|�B	��B	}�B	yB	vB	v�B	t�B	t_B	ueB	�LB	�@B	wB	t�B	s�B	t�B	t_B	t*B	qB	t�B	s�B	qB	pFB	oB	o�B	v�B	v7B	|�B	s�B	}-B	��B	B	v7B	t_B	rB	nnB	qLB	l�B	k(B	m4B	s�B	pFB	oB	sYB	ueB	v�B	�B	yIB	rB	p�B	o�B	o�B	m�B	p�B	q�B	q�B	t�B	z�B	yIB	z�B	~�B	z�B	xwB	B	�B	�kB	�B	�LB	��B	��B	�-B	��B	�qB	�B	��B	�9B	��B	��B	�EB	��B	��B	��B	��B	��B	�LB	�#B	��B	�RB	��B	�B	�B	��B	��B	��B	��B	��B	�XB	�XB	��B	�RB	��B	�RB	��B	�RB	��B	�^B	�gB	�B	�QB	��B	�B	�6B	��B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230730223107                            20230730223107AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023073022310720230730223107  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023073022310720230730223107QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023073022310720230730223107QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               