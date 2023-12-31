CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T00:00:56Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230721000056  20230721000056  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�9~R�^@�9~R�^11  @�9~����@�9~����@-Lnm��@-Lnm���c���E��c���E�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?u?��H@@  @�  @�G�@�p�@�  @��RA�RA   A,(�A@  A_\)A�  A�  A�  A�  A�Q�A�  A�  A�  A��B  B(�B  B   B((�B/�
B8(�B@Q�BH(�BP  BX  B_�
Bg�Bo�
Bx(�B�{B�(�B��B��B�  B�  B��B��
B��B�{B�  B�{B�  B��
B�{B���B��
B��B��B��B��B�  B�{B�{B�  B��B�  B�{B�{B�  B�  B�  C   C
=C  C��C  C	��C��C��C��C
=C
=C
=C  C
=C{C
=C   C"
=C$  C&  C(
=C*  C,  C-��C/��C1��C4  C6  C8  C:  C<  C=��C@  CB  CC��CF  CH  CI��CL  CN{CP{CR
=CS��CV
=CX{CZ
=C\  C]��C_�Cb  Cd
=Cf
=Ch
=Cj  Ck��Cm��Co�Cr  Ct{Cv  Cw��Cz  C|{C~{C�C���C���C���C�  C�
=C�  C���C�  C�  C�C�C�  C�
=C�
=C�C�  C�  C�  C�  C�C���C�  C�C�  C���C���C�  C�C�  C���C�C�C���C���C���C���C�  C�
=C�C�C�C�C�C�  C�  C�  C���C���C���C�  C���C���C�  C�  C�C�  C���C���C���C�C�  C�
=C�C���C���C�  C�C���C���C�  C�C�C�
=C�
=C�C���C�  C�C�
=C�
=C�  C���C���C���C���C�  C�  C�  C�  C���C�  C�C�C�  C�  C���C���C���C�C�  C���C���C���C�  C�  C�C�C�
=C�C���C�C�
=C�  C�C�C�  C�  C���C�C�
=C�
=C�C�C���C���C�C�
=D �D ��D �qD}qD  D� D�D��D  D}qD  D�D�D� D�D� D  D� D	  D	� D
  D
�DD}qD�qD��DD� D  D� D  D��D  Dz�D�qD��D  D}qD  D� D�qDz�D  D�D  Dz�D��D}qD�qD}qD�qD}qD  D��D�D��D  D}qD��D� D�D��D  D� D   D � D!�D!�D"  D"z�D"�qD#}qD$  D$��D$�qD%� D&�D&}qD&��D'xRD'�qD(��D)  D)� D*�D*�D+�D+� D+�qD,z�D-  D-� D-��D.}qD/�D/� D0  D0� D1  D1}qD1��D2� D3�D3�D4D4��D5�D5��D6D6��D7�D7��D8�D8� D8�qD9}qD9��D:z�D;  D;� D;�qD<��D=  D=}qD>�D>�D?D?�D@�D@� DA  DA}qDA�qDB� DC�DC� DC�qDD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL��DM�DM� DM�qDN}qDO�DO��DP�DP��DQ  DQ}qDQ�qDR��DS�DS��DT  DT}qDT�qDU}qDV  DV� DW  DW� DX  DX}qDX�qDY}qDZ  DZ� D[�D[��D\  D\� D\�qD]� D^  D^}qD_  D_� D`  D`� Da  Da� Db  Db� Dc�Dc� Dc�qDd� De  De}qDf�Df��Df�qDg� DhDh��Di�Di� Di�qDj}qDj�qDk� Dl�Dl� Dl�qDm}qDn�Dn�Do�Do}qDo�qDp��Dq�Dq� Dr  Dr� Ds  Ds��Dt  Dt� Du�Du� Du��Dv� DwDw� Dx  Dx�Dy  Dy� Dy�qDz� Dz�qD{� D|  D|}qD|��D}� D~D~� D  D��D�qD�>�D�~�D�� D�HD�AHD�� D���D���D�>�D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D��qD���D�@ D�� D��HD���D�@ D��HD�� D���D�@ D���D�� D���D�>�D�� D��HD�HD�@ D��HD�� D�  D�>�D�}qD�� D�HD�AHD�� D��HD�HD�AHD�� D���D��qD�=qD�~�D�� D���D�>�D��HD�D�HD�>�D�~�D��HD�HD�>�D�� D�� D�HD�B�D�� D���D�  D�@ D�� D���D�  D�B�D�� D�� D�HD�AHD�}qD��qD��qD�@ D��HD��HD�HD�AHD��HD�� D�  D�@ D��HD�D�HD�@ D�� D�� D�  D�>�D�}qD��qD���D�>�D�}qD���D���D�@ D�� D��HD�HD�B�D�� D���D�  D�AHD��HD�� D���D�@ D�� D��HD��D�AHD�� D��HD�  D�>�D�� D�D�HD�AHD�� D�� D�HD�AHD�� D��HD�HD�AHD�~�D���D�  D�@ D��HD�� D�  D�AHD�� D���D�  D�@ D��HD�D�  D�>�D��HD�D�HD�@ D�~�D��qD��qD�=qD�� D�D�HD�AHD�� D�� D�  D�AHD��HD�� D���D�>�D�� D�� D�  D�@ D�� D���D�  D�@ D��HD���D��qD�=qD�~�D���D���D�@ D�~�D��qD���D�@ D�� D���D�  D�B�D��HD���D��qD�>�D�� D�� D���D�@ D��HD��HD�  D�@ D�}qD��qD�  D�@ D�� D�� D���D�>�D�� D�� D�HD�B�D�� D�� D�HD�>�D�� D�D�  D�AHD��HD�� D�  D�B�D�� D���D�  D�AHD�� D���D��qD�>�D�~�D��HD��D�AHD�� D��HD�HD�AHD��HD�� D���D�>�D�� D���D�  D�@ D�~�D���D�  D�B�D D�� D�HD�@ DÀ Dþ�D���D�>�D�~�Dľ�D�  D�AHDŁHD��HD�  D�>�Dƀ D�� D�HD�AHDǀ DǾ�D���D�@ DȀ DȾ�D���D�>�Dɀ D�� D�  D�>�Dʀ D��HD���D�=qD�~�D�� D�HD�@ D�~�D̾�D�  D�@ D́HD�� D���D�@ D΀ D�� D�  D�@ Dπ D�� D�HD�AHDЁHD�� D�  D�>�Dр D��HD�  D�>�D�~�D�� D���D�@ DӀ DӾ�D�  D�@ DԀ D�� D�  D�@ DՁHD�� D�  D�AHDր D־�D�  D�@ DׁHD��HD�  D�AHD؀ D�� D�  D�>�D�~�D�� D�HD�AHDځHD��HD�  D�>�Dۀ D�� D���D�>�D܀ D�� D�HD�AHD݁HD�� D���D�@ Dހ D�� D�  D�AHD߁HD�� D�  D�>�D�� D�� D�  D�@ D� D�� D�HD�AHD� D�� D�HD�AHD�HD�� D�  D�=qD�~�D��HD�  D�AHD傏D��HD�  D�>�D�~�D澸D���D�>�D� D�� D�HD�@ D�~�D辸D���D�=qD�~�D�� D���D�>�D� D��HD�HD�@ D� D�� D�  D�=qD�}qD��HD�HD�>�D�~�D��HD��D�AHD� DD��qD�@ D�HD��HD�  D�>�D�~�D�� D�HD�@ D� D�� D�  D�@ D�HD�� D�  D�AHD�~�D�� D�  D�AHD� D���D�  D�AHD��HD�� D�  D�@ D�~�D���D�  D�>�D��HD�D�  D�@ D��HD�� D�HD�B�D�� D�� D�HD�(�D�e?��?.{?k�?���?\?�(�@�@
=@#�
@5@J=q@Y��@h��@�  @���@�{@���@��
@���@�33@�(�@���@У�@ٙ�@��
@�\)@�Q�A ��AA�A��A�A=qA   A%A)��A.�RA5�A9��A>{AC�
AH��AN{AR�\AXQ�A]p�Aa�AfffAl(�Aq�AvffAz=qA�Q�A��HA���A�\)A�=qA�z�A��RA���A��A�{A�  A��\A�p�A�\)A���A�(�A�ffA�Q�A��\A��A��A���A��
A��RA�G�A�33A�p�A�Q�A��HA���A�\)Aʏ\A�z�AθRA�G�A�(�A�ffA�Q�Aڏ\A��A߮A��A�(�A�ffA�G�A��
A�ffA�Q�A�33A�A�Q�A�=qA���A��B�B=qB�B��B=qB�B��B	B33Bz�B��B�RB  Bp�B�RB�
B��BffB�
B��B{B�B��B=qB\)B ��B!�B#\)B$z�B%B'33B(z�B)B*�HB,Q�B-B/
=B0  B1p�B3
=B4(�B5G�B6�HB8(�B9G�B:�\B<  B=p�B>�\B?�
BA�BB�RBD  BE�BFffBG�
BIG�BJffBK�BM�BN�\BO�BP��BRffBS�
BT��BV=qBW�BY�BZffB[�B\��B^=qB_�B`��Bb{Bc�Bd��Be�Bg33Bh��Bj{Bk33Blz�Bm�Bo\)Bpz�BqBs
=Btz�Bu�Bw
=BxQ�By��B{
=B|Q�B}��B~�HB�(�B���B�G�B�  B���B�\)B�  B��\B�33B��B�z�B�
=B�B�z�B�
=B��B�ffB��B��
B�z�B��B��
B��\B��B�B�ffB�33B��B�z�B��B��B��\B��B�B��\B�33B��B��\B�\)B�  B��RB�\)B�=qB���B���B�=qB���B�B�z�B�G�B��B��\B�G�B�(�B���B�p�B�(�B���B�B�ffB�
=B��
B���B�\)B�{B���B�p�B�=qB�
=B��
B��\B�G�B�  B��RB��B�=qB�
=B��B�Q�B�
=B��B�ffB���B��B��B�ffB���B�\)B��B��B�{B�ffB���B��HB���B��B��B�G�B��B���B��B�B��
B�  B�(�B�=qB�=qB�ffB�z�B��RB��HB���B�
=B��B�\)B�p�B���B��B�B��
B�{B�Q�B�ffB�z�B��\B��RB���B�33B�\)B�p�B��B�B�  B�(�B�Q�B�z�B\B���B�
=B�G�BÅBîB�B��B�=qB�z�BĸRB��HB�
=B�G�B�p�B�B�{B�=qB�z�BƸRB��HB��B�\)BǙ�B��
B�(�B�ffBȣ�B��HB�
=B�G�BɅBɮB�{B�=qBʏ\B���B���B�33B�p�BˮB��B�=qB̏\B���B�
=B�G�BͅB�B�  B�Q�BΣ�B���B�33BυB�B�  B�=qB�z�B���B��B�p�B��
B�{B�ffBҸRB��HB��B�p�B�B�{B�z�B���B��B�\)BծB��B�Q�B֣�B���B�\)B�B�{B�ffBأ�B���B�G�Bٙ�B��B�Q�Bڣ�B�
=B�\)B�B�{B�ffBܸRB�
=B�\)BݮB�  B�Q�Bޣ�B���B�\)B�B�{B�z�B��HB�33B�B��B�=qB�\B��HB�G�B㙚B�{B�z�B���B�33B噚B��B�Q�B��B���B�G�B�B�  B�ffB�RB�33B�B��B�Q�B��B�
=B�p�B��
B�(�B�\B���B�G�B��B�  B�Q�B�RB�
=B�p�B��
B�=qB��B�
=B�p�B��
B�=qB�\B���B�\)B�B�  B�ffB���B��B��B��
B�=qB���B���B�\)B�B�(�B��\B��HB�\)B��B�(�B��\B��HB�G�B���B�  B�ffB��RB��B��B��B�=qB��\B���B�G�B��C 
=C 33C ffC ��C C  C(�C\)C�\CC  C(�CQ�C�C�RC�C�CQ�C�C�RC�C�CQ�C�C�RC�C�CQ�C�C�RC�C�CQ�C�C�RC�C�CQ�C�C�RC��C�CQ�C�C�RC��C	�C	\)C	��C	C
  C
33C
p�C
��C
�
C{CQ�C�CC  C=qCz�C�C��C(�CffC��C�
C
=CG�Cz�C�RC�C�C\)C�\C��C  C=qCp�C�C�C�CffC��C�HC�CQ�C�\CC  C=qCp�C�C�C33CffC��C�HC(�C\)C�\CC��C(�CffC��C�HC�C\)C��C��C
=C=qCp�C�C�C(�CffC��C�HC�CQ�C�CC  C=qCz�C�C�C�CQ�C�CC  C=qCz�C�C�HC{CG�Cz�C�RC�C33Cp�C�RC�C (�C \)C �\C �
C!
=C!=qC!z�C!�RC!��C"33C"p�C"�C"�C#(�C#\)C#�\C#C$  C$33C$p�C$�C$�C%(�C%ffC%��C%�
C&
=C&=qC&�C&�RC'  C'=qC'z�C'�C'�C((�C(\)C(�\C(�
C){C)\)C)�\C)��C*  C*33C*p�C*�C*��C+33C+p�C+�C+�C,�C,ffC,�C,��C-=qC-z�C-�C-��C.33C.�C.C/  C/G�C/�C/C0{C0\)C0��C0�HC1�C1ffC1�C2  C2=qC2�C2C3  C3Q�C3��C3�HC4�C4Q�C4��C4��C533C5z�C5�RC5��C6=qC6�C6�
C7�C7Q�C7��C7�
C8(�C8p�C8�RC8��C933C9z�C9C:{C:\)C:��C:�C;(�C;�C;��C<�C<ffC<��C<��C=G�C=��C=�C>33C>z�C>C?�C?p�C?��C@{C@\)C@��CA  CA\)CA��CA�CB33CB�CB�HCC33CC�CC��CD{CDffCD�RCE{CEffCE��CE��CFG�CF��CF�CG=qCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                   ?u?��H@@  @�  @�G�@�p�@�  @��RA�RA   A,(�A@  A_\)A�  A�  A�  A�  A�Q�A�  A�  A�  A��B  B(�B  B   B((�B/�
B8(�B@Q�BH(�BP  BX  B_�
Bg�Bo�
Bx(�B�{B�(�B��B��B�  B�  B��B��
B��B�{B�  B�{B�  B��
B�{B���B��
B��B��B��B��B�  B�{B�{B�  B��B�  B�{B�{B�  B�  B�  C   C
=C  C��C  C	��C��C��C��C
=C
=C
=C  C
=C{C
=C   C"
=C$  C&  C(
=C*  C,  C-��C/��C1��C4  C6  C8  C:  C<  C=��C@  CB  CC��CF  CH  CI��CL  CN{CP{CR
=CS��CV
=CX{CZ
=C\  C]��C_�Cb  Cd
=Cf
=Ch
=Cj  Ck��Cm��Co�Cr  Ct{Cv  Cw��Cz  C|{C~{C�C���C���C���C�  C�
=C�  C���C�  C�  C�C�C�  C�
=C�
=C�C�  C�  C�  C�  C�C���C�  C�C�  C���C���C�  C�C�  C���C�C�C���C���C���C���C�  C�
=C�C�C�C�C�C�  C�  C�  C���C���C���C�  C���C���C�  C�  C�C�  C���C���C���C�C�  C�
=C�C���C���C�  C�C���C���C�  C�C�C�
=C�
=C�C���C�  C�C�
=C�
=C�  C���C���C���C���C�  C�  C�  C�  C���C�  C�C�C�  C�  C���C���C���C�C�  C���C���C���C�  C�  C�C�C�
=C�C���C�C�
=C�  C�C�C�  C�  C���C�C�
=C�
=C�C�C���C���C�C�
=D �D ��D �qD}qD  D� D�D��D  D}qD  D�D�D� D�D� D  D� D	  D	� D
  D
�DD}qD�qD��DD� D  D� D  D��D  Dz�D�qD��D  D}qD  D� D�qDz�D  D�D  Dz�D��D}qD�qD}qD�qD}qD  D��D�D��D  D}qD��D� D�D��D  D� D   D � D!�D!�D"  D"z�D"�qD#}qD$  D$��D$�qD%� D&�D&}qD&��D'xRD'�qD(��D)  D)� D*�D*�D+�D+� D+�qD,z�D-  D-� D-��D.}qD/�D/� D0  D0� D1  D1}qD1��D2� D3�D3�D4D4��D5�D5��D6D6��D7�D7��D8�D8� D8�qD9}qD9��D:z�D;  D;� D;�qD<��D=  D=}qD>�D>�D?D?�D@�D@� DA  DA}qDA�qDB� DC�DC� DC�qDD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL��DM�DM� DM�qDN}qDO�DO��DP�DP��DQ  DQ}qDQ�qDR��DS�DS��DT  DT}qDT�qDU}qDV  DV� DW  DW� DX  DX}qDX�qDY}qDZ  DZ� D[�D[��D\  D\� D\�qD]� D^  D^}qD_  D_� D`  D`� Da  Da� Db  Db� Dc�Dc� Dc�qDd� De  De}qDf�Df��Df�qDg� DhDh��Di�Di� Di�qDj}qDj�qDk� Dl�Dl� Dl�qDm}qDn�Dn�Do�Do}qDo�qDp��Dq�Dq� Dr  Dr� Ds  Ds��Dt  Dt� Du�Du� Du��Dv� DwDw� Dx  Dx�Dy  Dy� Dy�qDz� Dz�qD{� D|  D|}qD|��D}� D~D~� D  D��D�qD�>�D�~�D�� D�HD�AHD�� D���D���D�>�D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D��qD���D�@ D�� D��HD���D�@ D��HD�� D���D�@ D���D�� D���D�>�D�� D��HD�HD�@ D��HD�� D�  D�>�D�}qD�� D�HD�AHD�� D��HD�HD�AHD�� D���D��qD�=qD�~�D�� D���D�>�D��HD�D�HD�>�D�~�D��HD�HD�>�D�� D�� D�HD�B�D�� D���D�  D�@ D�� D���D�  D�B�D�� D�� D�HD�AHD�}qD��qD��qD�@ D��HD��HD�HD�AHD��HD�� D�  D�@ D��HD�D�HD�@ D�� D�� D�  D�>�D�}qD��qD���D�>�D�}qD���D���D�@ D�� D��HD�HD�B�D�� D���D�  D�AHD��HD�� D���D�@ D�� D��HD��D�AHD�� D��HD�  D�>�D�� D�D�HD�AHD�� D�� D�HD�AHD�� D��HD�HD�AHD�~�D���D�  D�@ D��HD�� D�  D�AHD�� D���D�  D�@ D��HD�D�  D�>�D��HD�D�HD�@ D�~�D��qD��qD�=qD�� D�D�HD�AHD�� D�� D�  D�AHD��HD�� D���D�>�D�� D�� D�  D�@ D�� D���D�  D�@ D��HD���D��qD�=qD�~�D���D���D�@ D�~�D��qD���D�@ D�� D���D�  D�B�D��HD���D��qD�>�D�� D�� D���D�@ D��HD��HD�  D�@ D�}qD��qD�  D�@ D�� D�� D���D�>�D�� D�� D�HD�B�D�� D�� D�HD�>�D�� D�D�  D�AHD��HD�� D�  D�B�D�� D���D�  D�AHD�� D���D��qD�>�D�~�D��HD��D�AHD�� D��HD�HD�AHD��HD�� D���D�>�D�� D���D�  D�@ D�~�D���D�  D�B�D D�� D�HD�@ DÀ Dþ�D���D�>�D�~�Dľ�D�  D�AHDŁHD��HD�  D�>�Dƀ D�� D�HD�AHDǀ DǾ�D���D�@ DȀ DȾ�D���D�>�Dɀ D�� D�  D�>�Dʀ D��HD���D�=qD�~�D�� D�HD�@ D�~�D̾�D�  D�@ D́HD�� D���D�@ D΀ D�� D�  D�@ Dπ D�� D�HD�AHDЁHD�� D�  D�>�Dр D��HD�  D�>�D�~�D�� D���D�@ DӀ DӾ�D�  D�@ DԀ D�� D�  D�@ DՁHD�� D�  D�AHDր D־�D�  D�@ DׁHD��HD�  D�AHD؀ D�� D�  D�>�D�~�D�� D�HD�AHDځHD��HD�  D�>�Dۀ D�� D���D�>�D܀ D�� D�HD�AHD݁HD�� D���D�@ Dހ D�� D�  D�AHD߁HD�� D�  D�>�D�� D�� D�  D�@ D� D�� D�HD�AHD� D�� D�HD�AHD�HD�� D�  D�=qD�~�D��HD�  D�AHD傏D��HD�  D�>�D�~�D澸D���D�>�D� D�� D�HD�@ D�~�D辸D���D�=qD�~�D�� D���D�>�D� D��HD�HD�@ D� D�� D�  D�=qD�}qD��HD�HD�>�D�~�D��HD��D�AHD� DD��qD�@ D�HD��HD�  D�>�D�~�D�� D�HD�@ D� D�� D�  D�@ D�HD�� D�  D�AHD�~�D�� D�  D�AHD� D���D�  D�AHD��HD�� D�  D�@ D�~�D���D�  D�>�D��HD�D�  D�@ D��HD�� D�HD�B�D�� D�� D�HD�(�D�e?��?.{?k�?���?\?�(�@�@
=@#�
@5@J=q@Y��@h��@�  @���@�{@���@��
@���@�33@�(�@���@У�@ٙ�@��
@�\)@�Q�A ��AA�A��A�A=qA   A%A)��A.�RA5�A9��A>{AC�
AH��AN{AR�\AXQ�A]p�Aa�AfffAl(�Aq�AvffAz=qA�Q�A��HA���A�\)A�=qA�z�A��RA���A��A�{A�  A��\A�p�A�\)A���A�(�A�ffA�Q�A��\A��A��A���A��
A��RA�G�A�33A�p�A�Q�A��HA���A�\)Aʏ\A�z�AθRA�G�A�(�A�ffA�Q�Aڏ\A��A߮A��A�(�A�ffA�G�A��
A�ffA�Q�A�33A�A�Q�A�=qA���A��B�B=qB�B��B=qB�B��B	B33Bz�B��B�RB  Bp�B�RB�
B��BffB�
B��B{B�B��B=qB\)B ��B!�B#\)B$z�B%B'33B(z�B)B*�HB,Q�B-B/
=B0  B1p�B3
=B4(�B5G�B6�HB8(�B9G�B:�\B<  B=p�B>�\B?�
BA�BB�RBD  BE�BFffBG�
BIG�BJffBK�BM�BN�\BO�BP��BRffBS�
BT��BV=qBW�BY�BZffB[�B\��B^=qB_�B`��Bb{Bc�Bd��Be�Bg33Bh��Bj{Bk33Blz�Bm�Bo\)Bpz�BqBs
=Btz�Bu�Bw
=BxQ�By��B{
=B|Q�B}��B~�HB�(�B���B�G�B�  B���B�\)B�  B��\B�33B��B�z�B�
=B�B�z�B�
=B��B�ffB��B��
B�z�B��B��
B��\B��B�B�ffB�33B��B�z�B��B��B��\B��B�B��\B�33B��B��\B�\)B�  B��RB�\)B�=qB���B���B�=qB���B�B�z�B�G�B��B��\B�G�B�(�B���B�p�B�(�B���B�B�ffB�
=B��
B���B�\)B�{B���B�p�B�=qB�
=B��
B��\B�G�B�  B��RB��B�=qB�
=B��B�Q�B�
=B��B�ffB���B��B��B�ffB���B�\)B��B��B�{B�ffB���B��HB���B��B��B�G�B��B���B��B�B��
B�  B�(�B�=qB�=qB�ffB�z�B��RB��HB���B�
=B��B�\)B�p�B���B��B�B��
B�{B�Q�B�ffB�z�B��\B��RB���B�33B�\)B�p�B��B�B�  B�(�B�Q�B�z�B\B���B�
=B�G�BÅBîB�B��B�=qB�z�BĸRB��HB�
=B�G�B�p�B�B�{B�=qB�z�BƸRB��HB��B�\)BǙ�B��
B�(�B�ffBȣ�B��HB�
=B�G�BɅBɮB�{B�=qBʏ\B���B���B�33B�p�BˮB��B�=qB̏\B���B�
=B�G�BͅB�B�  B�Q�BΣ�B���B�33BυB�B�  B�=qB�z�B���B��B�p�B��
B�{B�ffBҸRB��HB��B�p�B�B�{B�z�B���B��B�\)BծB��B�Q�B֣�B���B�\)B�B�{B�ffBأ�B���B�G�Bٙ�B��B�Q�Bڣ�B�
=B�\)B�B�{B�ffBܸRB�
=B�\)BݮB�  B�Q�Bޣ�B���B�\)B�B�{B�z�B��HB�33B�B��B�=qB�\B��HB�G�B㙚B�{B�z�B���B�33B噚B��B�Q�B��B���B�G�B�B�  B�ffB�RB�33B�B��B�Q�B��B�
=B�p�B��
B�(�B�\B���B�G�B��B�  B�Q�B�RB�
=B�p�B��
B�=qB��B�
=B�p�B��
B�=qB�\B���B�\)B�B�  B�ffB���B��B��B��
B�=qB���B���B�\)B�B�(�B��\B��HB�\)B��B�(�B��\B��HB�G�B���B�  B�ffB��RB��B��B��B�=qB��\B���B�G�B��C 
=C 33C ffC ��C C  C(�C\)C�\CC  C(�CQ�C�C�RC�C�CQ�C�C�RC�C�CQ�C�C�RC�C�CQ�C�C�RC�C�CQ�C�C�RC�C�CQ�C�C�RC��C�CQ�C�C�RC��C	�C	\)C	��C	C
  C
33C
p�C
��C
�
C{CQ�C�CC  C=qCz�C�C��C(�CffC��C�
C
=CG�Cz�C�RC�C�C\)C�\C��C  C=qCp�C�C�C�CffC��C�HC�CQ�C�\CC  C=qCp�C�C�C33CffC��C�HC(�C\)C�\CC��C(�CffC��C�HC�C\)C��C��C
=C=qCp�C�C�C(�CffC��C�HC�CQ�C�CC  C=qCz�C�C�C�CQ�C�CC  C=qCz�C�C�HC{CG�Cz�C�RC�C33Cp�C�RC�C (�C \)C �\C �
C!
=C!=qC!z�C!�RC!��C"33C"p�C"�C"�C#(�C#\)C#�\C#C$  C$33C$p�C$�C$�C%(�C%ffC%��C%�
C&
=C&=qC&�C&�RC'  C'=qC'z�C'�C'�C((�C(\)C(�\C(�
C){C)\)C)�\C)��C*  C*33C*p�C*�C*��C+33C+p�C+�C+�C,�C,ffC,�C,��C-=qC-z�C-�C-��C.33C.�C.C/  C/G�C/�C/C0{C0\)C0��C0�HC1�C1ffC1�C2  C2=qC2�C2C3  C3Q�C3��C3�HC4�C4Q�C4��C4��C533C5z�C5�RC5��C6=qC6�C6�
C7�C7Q�C7��C7�
C8(�C8p�C8�RC8��C933C9z�C9C:{C:\)C:��C:�C;(�C;�C;��C<�C<ffC<��C<��C=G�C=��C=�C>33C>z�C>C?�C?p�C?��C@{C@\)C@��CA  CA\)CA��CA�CB33CB�CB�HCC33CC�CC��CD{CDffCD�RCE{CEffCE��CE��CFG�CF��CF�CG=qCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�33A�/A�33A�1'A�-A�-A�-A�-A��A�$�A���A�  A���A���A��A��yA��TA��HA��HA���AѾwAѾwAѬAѣ�Aѣ�Aѣ�Aѡ�Aѡ�Aџ�Aѣ�Aѣ�Aѡ�Aѡ�Aѡ�Aџ�Aѝ�Aї�AхA�hsA�VA��;A̍PA���A�A�A�(�AǕ�A��A��#A�ZA��FA�VA���A���A��wA�^5A�  A�~�A��FA��DA��A�M�A�JA�ƨA���A�;dA��A��mA�
=A�A�/A��hA���A�+A��A�ĜA���A���A�jA�|�A��A��RA�O�A�r�A�VA~��AwƨAp�`Ak33Ag�wAa�
A]
=AY�mAUoAT5?AS�FASVAR$�AQ��APQ�AOAMAK��AK%AJ�AJ�jAIC�AHn�AG��AF�AE��AEt�AD�ADM�AB��A@�uA?�
A@-A@$�A@bA?��A>�uA=��A=hsA=G�A=G�A<^5A;G�A9hsA85?A7�TA7�
A7�hA6ȴA0�+A0$�A//A.v�A-�FA,�A,�!A,��A,ffA,VA+p�A*��A*$�A)�A(�HA(1A'�-A'�A&�yA&I�A%�hA%A$Q�A$ �A$A�A$�A#��A#\)A#oA"��A"$�A!�wA!|�A �A ~�A��A
=AA�A�AA�A��Av�A-A�A�#A�hA��A�/A�A{A�^A?}A&�A��A�RA�A�DA�Ar�AbNAbA�At�AhsAO�A�A�A1'A�#A��AS�A��A^5A�#A|�AoAȴA�A��A�AbNA{Ap�A/A�A�RA��AI�A��A��AS�AĜAn�AJA�;A�-AdZA33AȴA�AffA(�A�
A��At�AC�A
�HA
�+A
=qA	��A	�
A	�FA	�hA	�A�AĜA~�A{A�^At�AC�A
=A��A^5AA�A�A�-A�AO�A&�A�HA�jA��A�+AQ�A9XA-A �A�#AoA��A�AE�A1'A��A�wA;dA ~�A 9X@���@���@���@���@�z�@���@��y@��#@���@�r�@�A�@���@��P@�o@��@�7L@�&�@��@��/@���@�I�@���@�@�+@�V@�p�@���@�o@�v�@�-@��@�@�7L@��@�9X@�\@�ff@�^5@�=q@���@��@�ȴ@��@�j@�1'@�@�33@��@��@��@��m@�S�@�"�@��y@ް!@�ff@�M�@�5?@�-@�$�@�$�@��@ݲ-@� �@�dZ@�5?@ى7@؛�@�1'@ׅ@�ȴ@�{@Ցh@�7L@��@���@���@ӶF@�K�@�o@ҸR@�^5@�5?@��#@�`B@���@�I�@��@ϥ�@�K�@�33@��H@��@�@͙�@�p�@��@��`@̓u@�j@�Z@�(�@�\)@��H@�E�@���@ɲ-@�/@�z�@��
@�S�@��H@�^5@�J@�?}@ļj@�Q�@�  @î@�dZ@��@��y@���@°!@��@�&�@�Q�@�1'@�(�@���@�\)@���@�J@�p�@��`@�A�@�S�@�v�@��#@�O�@�G�@�&�@��@���@���@�j@�A�@��;@�o@��@��!@�M�@���@���@��h@�V@���@�I�@��
@�K�@�C�@�33@��y@�$�@��-@�p�@�V@���@�Q�@��m@���@�dZ@���@�=q@��7@�%@���@�1@���@��w@���@���@�t�@�\)@�@�ff@��T@�@��-@���@�r�@��@�ȴ@��@���@���@�p�@�/@���@��9@��u@�Q�@���@�;d@�
=@���@�-@��T@���@�`B@�O�@��@���@���@�r�@�I�@�1@�ƨ@���@�l�@�
=@�ff@�{@��T@��^@��7@�G�@��@���@��`@�Ĝ@��j@��9@��D@�Q�@�9X@��;@�dZ@��@�~�@�M�@�5?@���@��T@���@�%@��9@���@�r�@�1@���@��w@��P@�@���@��\@���@�/@���@��9@��u@�z�@�1'@��;@���@�;d@���@��@�`B@��/@��u@�Z@���@���@��w@���@�|�@�;d@��@��R@�n�@��#@�/@���@�Z@�1@���@���@�\)@�;d@��H@�n�@�5?@���@��#@���@�G�@�V@��j@�z�@�bN@��@���@���@��@�l�@�"�@��R@�V@�-@��T@�@���@���@��7@�O�@�&�@��`@�Q�@��;@��w@�t�@�@�5?@�J@���@��@���@���@�hs@�%@���@���@�I�@�(�@��@�;@�w@�w@|�@
=@~ff@~@}��@|��@|z�@|I�@|I�@{��@{t�@z�H@zn�@z�@y��@y�7@yhs@yG�@y%@x��@x�@x1'@x  @w�@wK�@v��@vff@u��@u/@t�/@tj@t1@s�F@sdZ@sC�@r�!@q��@q�^@q&�@p��@p1'@o��@n�@m��@m`B@mV@l(�@k��@k33@j^5@i��@iG�@h�`@h1'@g��@g
=@fff@e��@d�@dj@d(�@c��@c�m@cƨ@cdZ@c@b��@b�\@bM�@a��@ax�@a7L@`�u@`Q�@`b@_|�@_
=@^�y@^ff@]��@]�@\�@\I�@[��@[��@Z��@Z=q@Y�#@Y%@Xr�@XA�@X �@Xb@W�@W�@W��@Wl�@W;d@Vȴ@V5?@U��@U�-@U�@T�j@T�@T��@T�@T(�@S�m@S�
@SdZ@R�!@Rn�@R^5@R-@RJ@Q��@Q��@PĜ@P1'@O�@O��@O\)@O
=@N��@NV@M�@Mp�@L�/@L��@L�D@Lj@LI�@L�@K�F@K�@KS�@Ko@J^5@J�@IX@H�`@H��@H��@HA�@G��@G|�@G\)@F��@F��@Fff@F5?@E@E�-@E��@EO�@D�/@D�j@D�@D��@Dz�@DI�@C��@C�
@C��@CdZ@C"�@C@B��@B=q@A�@A�^@A�7@A7L@@�`@@Ĝ@@r�@@  @?\)@>�@>ȴ@>��@=�T@=��@=`B@=�@<�/@<��@<1@;t�@;@:��@:~�@:J@9��@9&�@8�u@8b@7|�@7\)@7+@6ȴ@6��@6ff@6{@5�@5�-@5`B@4��@4�D@3��@3ƨ@3t�@3C�@3@2��@2n�@2-@1��@1&�@1�@0��@0��@0�@0Q�@/�@/�@/;d@.�y@.��@.E�@.@-�@-�@-V@,�@+ƨ@+"�@*��@*��@*��@*�H@*�H@*��@*~�@*M�@*-@)��@)x�@(��@(��@(r�@(bN@(1'@(  @'�@'+@&��@&V@&E�@&$�@&$�@&{@%�-@%/@%/@$��@$z�@$(�@#dZ@#o@"�H@"n�@"-@!�@!�^@!��@ �`@ bN@ b@�w@�@v�@v�@V@$�@@��@�T@��@p�@O�@�j@I�@�@�
@�F@�@S�@"�@@�H@^5@��@�7@x�@G�@7L@�@Ĝ@Q�@ �@��@�@�P@��@�y@�@�@�R@v�@5?@@��@��@��@@@@�h@?}@��@�/@�j@��@j@I�@��@�
@�F@��@dZ@"�@�@��@�!@��@~�@^5@=q@�@J@J@J@�@��@G�@%@�u@bN@Q�@Q�@  @��@�w@�w@|�@;d@�y@�@ȴ@��@ff@E�@E�@$�@{@{@@�T@��@�@p�@�@V@�A�5?A�1'A�1'A�5?A�1'A�/A�/A�(�A�+A�33A�9XA�9XA�7LA�33A�+A�+A�-A�1'A�+A�+A�/A�/A�+A�+A�/A�1'A�/A�+A�+A�$�A��A��A�(�A�/A� �A���A�  A�  A���A���A�1A�1A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��`A��/A��A��;A��mA��TA��`A��`A��mA��yA��`A��TA��A���A��TA��yA��mA��;A��HA��yA��yA���A�ȴA�ȴA���A���A���A���A�ȴA�ĜAѼjAѺ^A�A�A���AѼjAѼjA���A���AѼjAѼjA���AѼjAѴ9Aѧ�Aѣ�Aѧ�Aѧ�Aѣ�Aѡ�Aѡ�Aѥ�Aѥ�Aѡ�Aѡ�Aѣ�Aѥ�Aѥ�Aѡ�Aѣ�Aѧ�Aѧ�Aѥ�Aѡ�Aѣ�Aѥ�Aѣ�Aѡ�Aѡ�Aѥ�Aѣ�Aџ�Aџ�Aѣ�Aѡ�Aџ�Aџ�Aѡ�Aѡ�Aѝ�Aџ�Aѡ�Aџ�Aѝ�Aѡ�Aѣ�Aѣ�Aџ�Aѡ�Aѥ�Aѣ�Aѡ�Aџ�Aѥ�Aѥ�Aѡ�Aџ�Aѥ�Aѥ�Aѣ�Aѡ�Aџ�Aѣ�Aѡ�Aџ�Aѡ�Aѣ�Aѣ�Aџ�Aѝ�Aџ�Aѡ�Aѡ�Aџ�Aѡ�Aѣ�Aѡ�Aѝ�Aџ�Aѡ�Aѝ�Aѝ�Aѝ�Aѡ�Aџ�Aћ�Aљ�Aћ�Aѝ�Aљ�AѓuAѕ�Aљ�Aѕ�Aя\AэPAэPAыDA�v�A�v�A�v�A�x�A�v�A�t�A�hsA�S�A�M�A�A�A�;dA�7LA�&�Aк^A�v�A��A΋DA͙�A� �A�%A��mA̺^Ḁ�A̙�A�~�A�n�A�?}A�+A��A��A˾wA�v�A�=qA���A�ȴA�bNA��A�M�A���A�bNA�"�A�  A��A���A���A���A�AǮA�9XA�A���AƩ�A�O�Aś�A�?}A��AĶFA�
=AÇ+A�
=A��A��HA�AA��A���A�33A��A��wA��7A�p�A���A�^5A�I�A�=qA�-A� �A�bA���A��/A���A�33A�
=A���A���A��A��/A���A���A���A���A�ĜA���A��FA���A��PA��+A�v�A�jA�bNA�VA�M�A�E�A�?}A�7LA�-A�"�A��A��A��A��A��A��A�VA�VA�bA�  A���A��A��A��-A��\A��A��A�t�A�ZA�9XA��A�  A��A��A��;A��9A�t�A�O�A�?}A�7LA�/A� �A�JA��`A��7A�C�A�(�A� �A�A���A�VA�A�ȴA���A�~�A�O�A��A��A�=qA���A��jA��9A���A���A��hA���A���A���A���A���A���A���A���A���A���A���A���A���A��uA���A���A��DA��A�|�A�K�A���A�ƨA��\A�?}A�1A��#A���A��FA��!A���A���A��7A�~�A�jA�K�A��A���A�ȴA�hsA��A��-A�x�A�E�A�33A�1'A�/A��A��A���A���A�XA�;dA�-A��`A���A�\)A���A��mA��A�dZA�7LA�%A��;A��RA���A��hA�x�A�ZA�G�A�$�A���A��
A���A��^A��9A���A���A��A�jA�dZA� �A�ȴA�bNA��TA�r�A�I�A�G�A�C�A�=qA�-A�{A��hA�E�A�oA���A��A��;A���A�ȴA��jA��A���A���A��uA��7A�v�A�O�A�oA���A���A��A�`BA�9XA�bA��;A��A��7A�ZA�C�A�9XA�+A�"�A��A�VA�A���A���A��A��A��A��A��A��yA��yA��mA��HA��;A��
A���A��9A���A��A�M�A�$�A��/A���A��hA��PA��+A��A�t�A�?}A�VA��A��A��TA��/A���A��jA��^A��-A���A���A��DA�t�A�A�A��yA���A���A��\A��A�`BA�/A��
A��uA��+A�r�A�S�A�+A��A��^A��DA�l�A�Q�A��yA�~�A�O�A�O�A�7LA��FA�33A��TA��!A��A�dZA�?}A� �A�A�A�l�A�9XA��A��A���A�=qA�%A��A���A��A���A��^A��!A�z�A�I�A��A���A�=qA�{A�%A���A�oA�jA���A�VA���A�C�A���A�G�A��A��#A��PA��A��
A��wA��7A��A�dZA�VA�"�A�bA��;A���A��\A�v�A�?}A�`BA��A�oA�Q�A���A�M�A�(�A��A���A���A�/A��A��FA�hsA�G�A���A�jA�
=A��RA�z�A�5?A�{A���A��yA���A�ZA�9XA�A���A���A�;dA�A�Q�A�bA��hA���A�S�A��A��`A��/A��A���A�Q�A�
=A�ĜA�~�A�`BA�I�A�9XA� �A�VA��A�^Ap�AS�AG�A;dA33A�A%A~�A~��A}��A}VA|bNA{�AyAw�7Av��Au�wAt~�As�
As33Ar��Ar(�Aq�^AqXAp�RAo�mAo33AnjAm�AlA�Ak��AkK�Ak%Aj�9Ajv�Aj5?Aj{Ai�AiAi\)Ah��Ag�^Af�Af$�AeC�Ad��AdbAc�Ab��AbbNAa��A`�yA`{A_G�A^�+A]�A]+A\�jA\�A\��A\��A\��A\��A\��A\bNA[��AZ9XAYK�AXbAV�AV�AV �AU��AU;dAT�yAT��ATz�ATVATQ�ATI�ATE�AT=qAT(�AT �AT �AT�AS��AS�;AS��ASAS��AS�7ASl�ASS�ASG�AS;dAS�ASVAR�AR��AR��ARffARI�AR=qAR�AR  AQ�AQ�TAQ�TAQ�
AQ��AQƨAQ�AQ��AQt�AQ;dAP��AP��AP�AP(�AO�AO��AO�FAO�PAO?}AO33AO"�AN�yAN�AM�AM`BAMVAL��AL�`AL��AL��ALI�AKƨAK��AKl�AKC�AK33AK�AKoAKoAKVAKAJ��AJ��AJ��AJ��AJ��AJ�AJ�AJ�AJ�AJ��AJ�AJ�AJ��AJv�AI�#AI�AI�7G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                   A�33A�/A�33A�1'A�-A�-A�-A�-A��A�$�A���A�  A���A���A��A��yA��TA��HA��HA���AѾwAѾwAѬAѣ�Aѣ�Aѣ�Aѡ�Aѡ�Aџ�Aѣ�Aѣ�Aѡ�Aѡ�Aѡ�Aџ�Aѝ�Aї�AхA�hsA�VA��;A̍PA���A�A�A�(�AǕ�A��A��#A�ZA��FA�VA���A���A��wA�^5A�  A�~�A��FA��DA��A�M�A�JA�ƨA���A�;dA��A��mA�
=A�A�/A��hA���A�+A��A�ĜA���A���A�jA�|�A��A��RA�O�A�r�A�VA~��AwƨAp�`Ak33Ag�wAa�
A]
=AY�mAUoAT5?AS�FASVAR$�AQ��APQ�AOAMAK��AK%AJ�AJ�jAIC�AHn�AG��AF�AE��AEt�AD�ADM�AB��A@�uA?�
A@-A@$�A@bA?��A>�uA=��A=hsA=G�A=G�A<^5A;G�A9hsA85?A7�TA7�
A7�hA6ȴA0�+A0$�A//A.v�A-�FA,�A,�!A,��A,ffA,VA+p�A*��A*$�A)�A(�HA(1A'�-A'�A&�yA&I�A%�hA%A$Q�A$ �A$A�A$�A#��A#\)A#oA"��A"$�A!�wA!|�A �A ~�A��A
=AA�A�AA�A��Av�A-A�A�#A�hA��A�/A�A{A�^A?}A&�A��A�RA�A�DA�Ar�AbNAbA�At�AhsAO�A�A�A1'A�#A��AS�A��A^5A�#A|�AoAȴA�A��A�AbNA{Ap�A/A�A�RA��AI�A��A��AS�AĜAn�AJA�;A�-AdZA33AȴA�AffA(�A�
A��At�AC�A
�HA
�+A
=qA	��A	�
A	�FA	�hA	�A�AĜA~�A{A�^At�AC�A
=A��A^5AA�A�A�-A�AO�A&�A�HA�jA��A�+AQ�A9XA-A �A�#AoA��A�AE�A1'A��A�wA;dA ~�A 9X@���@���@���@���@�z�@���@��y@��#@���@�r�@�A�@���@��P@�o@��@�7L@�&�@��@��/@���@�I�@���@�@�+@�V@�p�@���@�o@�v�@�-@��@�@�7L@��@�9X@�\@�ff@�^5@�=q@���@��@�ȴ@��@�j@�1'@�@�33@��@��@��@��m@�S�@�"�@��y@ް!@�ff@�M�@�5?@�-@�$�@�$�@��@ݲ-@� �@�dZ@�5?@ى7@؛�@�1'@ׅ@�ȴ@�{@Ցh@�7L@��@���@���@ӶF@�K�@�o@ҸR@�^5@�5?@��#@�`B@���@�I�@��@ϥ�@�K�@�33@��H@��@�@͙�@�p�@��@��`@̓u@�j@�Z@�(�@�\)@��H@�E�@���@ɲ-@�/@�z�@��
@�S�@��H@�^5@�J@�?}@ļj@�Q�@�  @î@�dZ@��@��y@���@°!@��@�&�@�Q�@�1'@�(�@���@�\)@���@�J@�p�@��`@�A�@�S�@�v�@��#@�O�@�G�@�&�@��@���@���@�j@�A�@��;@�o@��@��!@�M�@���@���@��h@�V@���@�I�@��
@�K�@�C�@�33@��y@�$�@��-@�p�@�V@���@�Q�@��m@���@�dZ@���@�=q@��7@�%@���@�1@���@��w@���@���@�t�@�\)@�@�ff@��T@�@��-@���@�r�@��@�ȴ@��@���@���@�p�@�/@���@��9@��u@�Q�@���@�;d@�
=@���@�-@��T@���@�`B@�O�@��@���@���@�r�@�I�@�1@�ƨ@���@�l�@�
=@�ff@�{@��T@��^@��7@�G�@��@���@��`@�Ĝ@��j@��9@��D@�Q�@�9X@��;@�dZ@��@�~�@�M�@�5?@���@��T@���@�%@��9@���@�r�@�1@���@��w@��P@�@���@��\@���@�/@���@��9@��u@�z�@�1'@��;@���@�;d@���@��@�`B@��/@��u@�Z@���@���@��w@���@�|�@�;d@��@��R@�n�@��#@�/@���@�Z@�1@���@���@�\)@�;d@��H@�n�@�5?@���@��#@���@�G�@�V@��j@�z�@�bN@��@���@���@��@�l�@�"�@��R@�V@�-@��T@�@���@���@��7@�O�@�&�@��`@�Q�@��;@��w@�t�@�@�5?@�J@���@��@���@���@�hs@�%@���@���@�I�@�(�@��@�;@�w@�w@|�@
=@~ff@~@}��@|��@|z�@|I�@|I�@{��@{t�@z�H@zn�@z�@y��@y�7@yhs@yG�@y%@x��@x�@x1'@x  @w�@wK�@v��@vff@u��@u/@t�/@tj@t1@s�F@sdZ@sC�@r�!@q��@q�^@q&�@p��@p1'@o��@n�@m��@m`B@mV@l(�@k��@k33@j^5@i��@iG�@h�`@h1'@g��@g
=@fff@e��@d�@dj@d(�@c��@c�m@cƨ@cdZ@c@b��@b�\@bM�@a��@ax�@a7L@`�u@`Q�@`b@_|�@_
=@^�y@^ff@]��@]�@\�@\I�@[��@[��@Z��@Z=q@Y�#@Y%@Xr�@XA�@X �@Xb@W�@W�@W��@Wl�@W;d@Vȴ@V5?@U��@U�-@U�@T�j@T�@T��@T�@T(�@S�m@S�
@SdZ@R�!@Rn�@R^5@R-@RJ@Q��@Q��@PĜ@P1'@O�@O��@O\)@O
=@N��@NV@M�@Mp�@L�/@L��@L�D@Lj@LI�@L�@K�F@K�@KS�@Ko@J^5@J�@IX@H�`@H��@H��@HA�@G��@G|�@G\)@F��@F��@Fff@F5?@E@E�-@E��@EO�@D�/@D�j@D�@D��@Dz�@DI�@C��@C�
@C��@CdZ@C"�@C@B��@B=q@A�@A�^@A�7@A7L@@�`@@Ĝ@@r�@@  @?\)@>�@>ȴ@>��@=�T@=��@=`B@=�@<�/@<��@<1@;t�@;@:��@:~�@:J@9��@9&�@8�u@8b@7|�@7\)@7+@6ȴ@6��@6ff@6{@5�@5�-@5`B@4��@4�D@3��@3ƨ@3t�@3C�@3@2��@2n�@2-@1��@1&�@1�@0��@0��@0�@0Q�@/�@/�@/;d@.�y@.��@.E�@.@-�@-�@-V@,�@+ƨ@+"�@*��@*��@*��@*�H@*�H@*��@*~�@*M�@*-@)��@)x�@(��@(��@(r�@(bN@(1'@(  @'�@'+@&��@&V@&E�@&$�@&$�@&{@%�-@%/@%/@$��@$z�@$(�@#dZ@#o@"�H@"n�@"-@!�@!�^@!��@ �`@ bN@ b@�w@�@v�@v�@V@$�@@��@�T@��@p�@O�@�j@I�@�@�
@�F@�@S�@"�@@�H@^5@��@�7@x�@G�@7L@�@Ĝ@Q�@ �@��@�@�P@��@�y@�@�@�R@v�@5?@@��@��@��@@@@�h@?}@��@�/@�j@��@j@I�@��@�
@�F@��@dZ@"�@�@��@�!@��@~�@^5@=q@�@J@J@J@�@��@G�@%@�u@bN@Q�@Q�@  @��@�w@�w@|�@;d@�y@�@ȴ@��@ff@E�@E�@$�@{@{@@�T@��@�@p�@�@V@�A�5?A�1'A�1'A�5?A�1'A�/A�/A�(�A�+A�33A�9XA�9XA�7LA�33A�+A�+A�-A�1'A�+A�+A�/A�/A�+A�+A�/A�1'A�/A�+A�+A�$�A��A��A�(�A�/A� �A���A�  A�  A���A���A�1A�1A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��`A��/A��A��;A��mA��TA��`A��`A��mA��yA��`A��TA��A���A��TA��yA��mA��;A��HA��yA��yA���A�ȴA�ȴA���A���A���A���A�ȴA�ĜAѼjAѺ^A�A�A���AѼjAѼjA���A���AѼjAѼjA���AѼjAѴ9Aѧ�Aѣ�Aѧ�Aѧ�Aѣ�Aѡ�Aѡ�Aѥ�Aѥ�Aѡ�Aѡ�Aѣ�Aѥ�Aѥ�Aѡ�Aѣ�Aѧ�Aѧ�Aѥ�Aѡ�Aѣ�Aѥ�Aѣ�Aѡ�Aѡ�Aѥ�Aѣ�Aџ�Aџ�Aѣ�Aѡ�Aџ�Aџ�Aѡ�Aѡ�Aѝ�Aџ�Aѡ�Aџ�Aѝ�Aѡ�Aѣ�Aѣ�Aџ�Aѡ�Aѥ�Aѣ�Aѡ�Aџ�Aѥ�Aѥ�Aѡ�Aџ�Aѥ�Aѥ�Aѣ�Aѡ�Aџ�Aѣ�Aѡ�Aџ�Aѡ�Aѣ�Aѣ�Aџ�Aѝ�Aџ�Aѡ�Aѡ�Aџ�Aѡ�Aѣ�Aѡ�Aѝ�Aџ�Aѡ�Aѝ�Aѝ�Aѝ�Aѡ�Aџ�Aћ�Aљ�Aћ�Aѝ�Aљ�AѓuAѕ�Aљ�Aѕ�Aя\AэPAэPAыDA�v�A�v�A�v�A�x�A�v�A�t�A�hsA�S�A�M�A�A�A�;dA�7LA�&�Aк^A�v�A��A΋DA͙�A� �A�%A��mA̺^Ḁ�A̙�A�~�A�n�A�?}A�+A��A��A˾wA�v�A�=qA���A�ȴA�bNA��A�M�A���A�bNA�"�A�  A��A���A���A���A�AǮA�9XA�A���AƩ�A�O�Aś�A�?}A��AĶFA�
=AÇ+A�
=A��A��HA�AA��A���A�33A��A��wA��7A�p�A���A�^5A�I�A�=qA�-A� �A�bA���A��/A���A�33A�
=A���A���A��A��/A���A���A���A���A�ĜA���A��FA���A��PA��+A�v�A�jA�bNA�VA�M�A�E�A�?}A�7LA�-A�"�A��A��A��A��A��A��A�VA�VA�bA�  A���A��A��A��-A��\A��A��A�t�A�ZA�9XA��A�  A��A��A��;A��9A�t�A�O�A�?}A�7LA�/A� �A�JA��`A��7A�C�A�(�A� �A�A���A�VA�A�ȴA���A�~�A�O�A��A��A�=qA���A��jA��9A���A���A��hA���A���A���A���A���A���A���A���A���A���A���A���A���A��uA���A���A��DA��A�|�A�K�A���A�ƨA��\A�?}A�1A��#A���A��FA��!A���A���A��7A�~�A�jA�K�A��A���A�ȴA�hsA��A��-A�x�A�E�A�33A�1'A�/A��A��A���A���A�XA�;dA�-A��`A���A�\)A���A��mA��A�dZA�7LA�%A��;A��RA���A��hA�x�A�ZA�G�A�$�A���A��
A���A��^A��9A���A���A��A�jA�dZA� �A�ȴA�bNA��TA�r�A�I�A�G�A�C�A�=qA�-A�{A��hA�E�A�oA���A��A��;A���A�ȴA��jA��A���A���A��uA��7A�v�A�O�A�oA���A���A��A�`BA�9XA�bA��;A��A��7A�ZA�C�A�9XA�+A�"�A��A�VA�A���A���A��A��A��A��A��A��yA��yA��mA��HA��;A��
A���A��9A���A��A�M�A�$�A��/A���A��hA��PA��+A��A�t�A�?}A�VA��A��A��TA��/A���A��jA��^A��-A���A���A��DA�t�A�A�A��yA���A���A��\A��A�`BA�/A��
A��uA��+A�r�A�S�A�+A��A��^A��DA�l�A�Q�A��yA�~�A�O�A�O�A�7LA��FA�33A��TA��!A��A�dZA�?}A� �A�A�A�l�A�9XA��A��A���A�=qA�%A��A���A��A���A��^A��!A�z�A�I�A��A���A�=qA�{A�%A���A�oA�jA���A�VA���A�C�A���A�G�A��A��#A��PA��A��
A��wA��7A��A�dZA�VA�"�A�bA��;A���A��\A�v�A�?}A�`BA��A�oA�Q�A���A�M�A�(�A��A���A���A�/A��A��FA�hsA�G�A���A�jA�
=A��RA�z�A�5?A�{A���A��yA���A�ZA�9XA�A���A���A�;dA�A�Q�A�bA��hA���A�S�A��A��`A��/A��A���A�Q�A�
=A�ĜA�~�A�`BA�I�A�9XA� �A�VA��A�^Ap�AS�AG�A;dA33A�A%A~�A~��A}��A}VA|bNA{�AyAw�7Av��Au�wAt~�As�
As33Ar��Ar(�Aq�^AqXAp�RAo�mAo33AnjAm�AlA�Ak��AkK�Ak%Aj�9Ajv�Aj5?Aj{Ai�AiAi\)Ah��Ag�^Af�Af$�AeC�Ad��AdbAc�Ab��AbbNAa��A`�yA`{A_G�A^�+A]�A]+A\�jA\�A\��A\��A\��A\��A\��A\bNA[��AZ9XAYK�AXbAV�AV�AV �AU��AU;dAT�yAT��ATz�ATVATQ�ATI�ATE�AT=qAT(�AT �AT �AT�AS��AS�;AS��ASAS��AS�7ASl�ASS�ASG�AS;dAS�ASVAR�AR��AR��ARffARI�AR=qAR�AR  AQ�AQ�TAQ�TAQ�
AQ��AQƨAQ�AQ��AQt�AQ;dAP��AP��AP�AP(�AO�AO��AO�FAO�PAO?}AO33AO"�AN�yAN�AM�AM`BAMVAL��AL�`AL��AL��ALI�AKƨAK��AKl�AKC�AK33AK�AKoAKoAKVAKAJ��AJ��AJ��AJ��AJ��AJ�AJ�AJ�AJ�AJ��AJ�AJ�AJ��AJv�AI�#AI�AI�7G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
,�B
,�B
,qB
-�B
-B
-B
,�B
-B
.B
,�B
0UB
,�B
,qB
,�B
,�B
,�B
,qB
,=B
-B
,=B
,B
+�B
,qB
+�B
+�B
+�B
,B
+�B
+�B
+�B
+�B
+�B
+�B
+6B
+B
*eB
)_B
(XB
%�B
#:B
33B
$B
/B
HB
Z�B
bB
��B
��B
�B
�dB!�B&B4�BW?B��B�hBƨB��BŢB�WB�BB�9B��B�tB��B��B��B�IB�@B�rB|�Bo5B^jBHB$�B;B
ȀB
��B
iB
<6B
�B	�	B	ѷB	�HB	��B	��B	�B	� B	��B	�4B	�B	�*B	��B	�nB	�B	�BB	�0B	҉B	�B
�B
.B
A B
@�B
?�B
A�B
P}B
]/B
iyB
tB
y�B
��B
�B
�B
��B
��B
.B
�oB
��B
�aB
�XB
�)B
��B
��B
�5B
�
B
��B
�fB
�)B
ǮB
�OB
�B
�LB
��B
�B
j�B
e`B
_B
\]B
[�B
ZB
^�B
a�B
h�B
gmB
b�B
^�B
\)B
^B
YB
W�B
W�B
X�B
YKB
[WB
[�B
Z�B
ZQB
\�B
c�B
d�B
c�B
a�B
c B
]�B
YB
XyB
V�B
T�B
U�B
T�B
TaB
S�B
S�B
S�B
UgB
U�B
U�B
XyB
YB
]�B
]�B
\�B
`B
^B
^B
\�B
\�B
^�B
\�B
_B
d&B
e�B
g8B
gmB
h
B
iB
iyB
iB
iDB
iB
j�B
i�B
g�B
f�B
d�B
b�B
_�B
_B
]�B
]�B
\�B
[�B
[�B
[#B
Y�B
YB
V9B
U2B
UgB
TaB
T,B
T�B
T�B
TaB
T,B
U2B
T,B
T,B
S&B
S&B
S[B
S[B
S&B
Q�B
Q�B
Q�B
Q�B
P�B
P�B
P}B
Q�B
RTB
RTB
R�B
R B
Q�B
Q�B
R�B
P�B
P�B
P�B
P}B
O�B
OBB
N�B
N�B
NpB
MB
MB
MB
K�B
K�B
J�B
J�B
J�B
JXB
I�B
J�B
I�B
I�B
H�B
H�B
HKB
IRB
E9B
EB
C�B
CaB
C�B
B'B
D3B
@�B
?B
=qB
=�B
;�B
;0B
6�B
6FB
4�B
3hB
1�B
0�B
1'B
2�B
1�B
0�B
0�B
-CB
,�B
,�B
,qB
,B
,B
+B
)�B
*�B
)�B
(�B
%zB
!�B
 'B
OB
�B
�B
IB
CB
!B
�B
�B
�B
�B
YB
�B
�B
�B
B
�B
@B
 B
.B
bB
�B
FB
�B
hB
B
FB
�B
FB
FB
B
B
uB
�B
�B
bB
�B
�B
�B
(B
"B
�B
B
JB
"B
B
�B
�B
�B
"B
VB
�B
VB
�B
�B
�B
PB
�B
�B
�B
"B
�B
B
�B
(B
�B
�B
�B
(B
�B
(B
�B
�B
�B
.B
"B
�B
VB
VB
"B
�B
�B
bB
4B
�B
 B
 B
 B
4B
�B
 B
 B
hB
�B
�B
�B
�B
B
@B
B
�B
B
�B
�B
B
�B
B
�B
�B
�B
�B
B
B
uB
�B
�B
�B
uB
@B
�B
�B
FB
{B
B
B
B
B
�B
�B
�B
SB
�B
�B
SB
YB
�B
�B
+B
_B
+B
�B
�B
�B
�B
�B
eB
	B
	B
�B
IB
~B
�B
�B
�B
B
B
 �B
!bB
!bB
 �B
 �B
!bB
!�B
"hB
!bB
#�B
#B
#B
#�B
$@B
$�B
%B
$�B
%FB
&�B
&B
&B
'B
($B
'�B
(XB
(XB
(�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
+B
+�B
,=B
,B
,=B
,B
,�B
,�B
,�B
-B
,�B
-B
,�B
,�B
-CB
-CB
,�B
.B
.B
.�B
.IB
.�B
.}B
.}B
.IB
.B
/�B
/OB
.�B
/B
/�B
/OB
.�B
/�B
0�B
/�B
/�B
1�B
1�B
1[B
1[B
1'B
1'B
1�B
1�B
2-B
1�B
2�B
4B
3�B
4�B
4�B
5?B
5?B
5B
5B
5B
5?B
5tB
5�B
5�B
6B
6�B
7�B
8B
8�B
8�B
8�B
9�B
9�B
9�B
:*B
:�B
:�B
;0B
;0B
;�B
<6B
<jB
<�B
<jB
<jB
=<B
=B
=�B
=<B
=qB
>B
>�B
>�B
?HB
?}B
?}B
?}B
?}B
?HB
?�B
?}B
?�B
AUB
AUB
A B
A�B
C-B
CaB
C�B
C�B
C�B
C�B
C�B
C�B
E9B
D�B
EmB
FB
FB
FB
F�B
FtB
FtB
GB
G�B
HKB
G�B
HB
G�B
HKB
HKB
HB
H�B
IRB
I�B
I�B
I�B
JXB
J#B
JXB
JXB
J�B
K^B
K^B
K�B
K^B
K�B
K�B
K�B
LdB
LdB
MB
L�B
MjB
M6B
MjB
M�B
M6B
N<B
N<B
NpB
N�B
N�B
OB
O�B
PB
QNB
PHB
P�B
QB
Q�B
Q�B
R�B
R�B
RTB
R�B
S&B
S&B
S�B
T,B
T�B
U2B
U�B
U�B
U�B
U�B
U�B
V9B
VB
V9B
V9B
VmB
VmB
VmB
V�B
W?B
W?B
W?B
W�B
W�B
WsB
XB
XEB
W�B
XEB
XyB
YKB
YKB
ZB
YB
Y�B
Z�B
ZB
ZQB
Y�B
ZQB
ZB
[#B
\�B
\�B
\�B
]dB
]�B
]/B
\�B
]dB
\�B
^B
^jB
^�B
_;B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_pB
_pB
_�B
_pB
_;B
_pB
_pB
_�B
`B
`B
`vB
aHB
aHB
aHB
aHB
a|B
aHB
a|B
a�B
a�B
a�B
a�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
dZB
d�B
dZB
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
ffB
ffB
ffB
f�B
gB
f�B
gB
gmB
gmB
g�B
g�B
h>B
hsB
hsB
h�B
h�B
h�B
h�B
iB
iDB
jB
jB
i�B
jB
jB
jB
jB
j�B
j�B
j�B
kQB
kQB
l"B
lWB
l�B
l�B
l�B
m]B
m]B
m�B
n/B
m�B
ncB
n�B
n�B
o B
o5B
oiB
oiB
o�B
pB
p;B
p;B
p�B
qAB
qvB
rB
rB
rGB
rGB
sMB
sB
sMB
s�B
s�B
s�B
s�B
tTB
tB
t�B
t�B
u%B
u�B
u�B
u�B
u�B
u�B
v+B
u�B
u%B
t�B
u%B
uZB
v+B
v�B
x8B
xB
xlB
xlB
xlB
y>B
y�B
y�B
y�B
zB
y�B
zB
zDB
z�B
{JB
{JB
{JB
{JB
{B
{B
{JB
|B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~]B
~�B
~�B
~]B
.B
.B
�B
�B
cB
.B
�B
��B
��B
��B
��B
�B
�oB
�oB
�B
�AB
�uB
��B
��B
��B
��B
��B
�GB
�GB
�B
�B
�MB
�MB
��B
��B
�B
�MB
��B
��B
�SB
��B
��B
��B
��B
��B
��B
��B
��B
�_B
��B
�1B
�1B
�fB
�1B
�fB
�1B
�1B
��B
�B
�B
�7B
�7B
��B
��B
��B
�	B
�	B
�	B
�	B
�	B
�=B
�=B
��B
�rB
��B
�B
�B
�DB
�DB
�xB
�xB
�xB
�xB
�xB
�~B
�~B
�PB
��B
�PB
�PB
��B
��B
�VB
�VB
��B
�VB
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
�.B
��B
� B
�4B
�4B
�B
��B
�.B
+B
-�B
+�B
-�B
-�B
.IB
.�B
,qB
.}B
,B
(XB
,qB
,�B
/B
-�B
-�B
-B
,B
-wB
.IB
,�B
+�B
-wB
-�B
,qB
,B
,�B
-CB
.�B
+�B
.B
/B
-B
*�B
.IB
6�B
+�B
,B
-wB
,qB
+6B
+6B
.�B
-B
+�B
,�B
-wB
,qB
,=B
+�B
-�B
,=B
+�B
,�B
-�B
+6B
)�B
.�B
,�B
-B
,=B
,=B
-�B
,qB
+�B
+�B
-�B
-CB
-�B
-CB
+kB
*eB
-CB
-wB
+�B
+kB
*�B
.}B
,�B
+�B
,qB
,=B
*0B
*eB
.�B
,qB
*�B
,qB
4nB
1[B
,�B
+�B
+�B
,=B
-�B
+kB
-�B
,=B
,�B
+�B
*�B
+�B
,=B
,�B
+kB
+6B
,=B
,�B
+6B
,�B
,=B
0�B
,qB
+kB
,B
+�B
,�B
,qB
+�B
+6B
,qB
,�B
+�B
+B
+�B
,�B
,=B
+B
+B
+�B
,�B
,B
*�B
,B
,�B
,qB
*�B
,B
,�B
,qB
*�B
+kB
,�B
,=B
+�B
+B
,�B
,B
+B
,B
,�B
+�B
+6B
+6B
,�B
,�B
*�B
+6B
,�B
,qB
+B
+6B
,qB
,=B
*�B
*0B
+�B
,qB
+�B
*�B
+6B
,�B
+�B
+6B
*�B
,B
,�B
*�B
)�B
+6B
,B
+6B
)�B
+6B
,=B
+kB
)�B
*�B
+�B
+kB
)�B
)�B
*�B
+6B
)�B
)_B
)�B
*�B
)_B
(XB
'�B
)�B
)�B
'RB
&�B
,�B
&�B
&�B
$�B
%B
%�B
)�B
$B
"�B
#nB
#:B
�B
 �B
/OB
%�B
_;B
,B
7�B
*�B
&B
$tB
%B
!�B
!�B
!bB
"hB
)�B
(XB
,=B
)_B
5?B
4B
6zB
:�B
8�B
IB
R�B
Z�B
ZB
_�B
Z�B
X�B
[�B
Z�B
Z�B
[�B
[�B
_�B
v+B
m]B
t�B
s�B
�{B
��B
�xB
�@B
�RB
��B
��B
͟B
�aB
��B
��B
ȀB
��B
��B
��B
��B
ɆB
��B+B(�B,B!BIB~BB!B �B"�B)_B:^B)*B)�B*0B*eB,qB-�B.IB-�B-wB-�B-CB0UB3hB9$B9�BB�BA�BB�BCaBB[BAUB@�B@�BA�BA�B?�BA BC�BE9BE9BF?BMBNBOBVmBY�BZ�B`�Bc�Bf2Bd�Bd&Bd�Be�Bf�BkQBn�Bp�Bp;Bu%B|B�lB��B��B�~B�IB�!B�'B�LB��B��B��B��B�FB�_B�B�6B�LB��B��B��B��B��B��B�zB��B��B��B��B��B��B�*B�B�wB�wB��B��B�HB��B�[B�B�?B�?B�?B��B�B�?B��B��B��B֡B� B� B�}B�B�zB��B��BǮBȀB�BȀBǮB��B�B��B�6B��BҽB�B͟B�B��B�*B��B�0B��B�EB��BϫB�2B�B�NB��B� B�B��B�sB�jBںB��B�KB�sB�2B�B��BΥB�6B�dB�B��B�BǮB��B�zB��B��BŢB��B��B��B�9BƨB�jB��B��B��B��B�B��B�hB��B��B�tB��B��B�nB�FB�zB��B�RB��B��B��B�B�nB�FB�*B�?B�B��B��B�B��B��B�eB�*B�kB�:B��B�B�B��B�bB�bB�!B��B�'B��B�bB�bB��B�-B�bB��B�4B��B�:B�hB�-B��B��B�bB�IB��B��B�MB��B�{B��B��B��B�YB�@B�\B��B��B��B�(B��B��B��B�B��B��B��B�7B��B��B�uB�uB�B��B��B}�Bz�Bw�Bv�By	Bw�Bt�BqvBl�Bn�By>Bq�Bd�BbBg�BtBpoBe�Bd&Ba�B[�B]dBXEBX�B^B[�BU�BM�BQ�BX�BM�BF�BC�BC�BDgB0�B0�B,�B/�B+kB+�B0�B�BB(B+BCB
�B�BB�B
��B
��B
��B
�B
�aB
�B
��B
�-B
�OB
�9B
ޞB
�[B
�zB
��B
�zB
��B
�\B
�~B
��B
�{B
��B
�B
�xB
{B
s�B
m)B
YB
R�B
S�B
^B
V�B
FB
O�B
?B
:�B
AUB
HKB
$tB
0�B
�B
~B
�B
hB
\B
�B
�B
YB

	B	��B	��B
�B	��B	��B	��B	�B	�%B	��B	ݘB	��B	�BB	�&B	˒B	�[B	��B	҉B	��B	�3B	B	�OB	�}B	�wB	�B	��B	��B	��B	�zB	�FB	��B	�3B	��B	�-B	�hB	��B	��B	��B	�*B	�aB	��B	��B	��B	��B	�1B	��B	��B	��B	��B	�YB	�JB	��B	��B	�B	��B	�\B	�oB	{JB	|�B	z�B	zDB	xB	v�B	v`B	s�B	{�B	~�B	�	B	��B	�	B	��B	��B	��B	�rB	�rB	��B	��B	�{B	�	B	�B	��B	��B	�FB	��B	�_B	��B	��B	��B	��B	�SB	��B	��B	�jB	�hB	�OB	�!B	��B	��B	��B	�9B	��B	�B	�nB	��B	�-B	��B	�hB	�B	��B	��B	�B	�tB	��B	��B	�FB	�tB	�B	��B	��B	�^B	�$B	�0B	��B	�jB	� B	��B	ǮB	̘B	��B	�B	�<B	�jB	ΥB	��B	�pB	�vB	�<B	ϫB	�&B	��B	�sB	خB	��B	�B	�8B	�oB	�oB	��B	��B	�DB
�B
B
 4B
�B
 B
B
#nB
1'B
/OB
0!B
1�B
5�B
>�B
G�B
AUB
AUB
AUB
A�B
C-B
?�B
@�B
?HB
A�B
B�B
A B
@�B
?HB
?}B
?�B
@B
?�B
>BB
=�B
=qB
<�B
?�B
GzB
IRB
I�B
K^G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                   B
&WB
&�B
&#B
'^B
&�B
&�B
&�B
&�B
'�B
&WB
*B
&WB
&#B
&WB
&WB
&�B
&#B
%�B
&�B
%�B
%�B
%�B
&#B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%QB
%QB
%QB
$�B
$�B
$B
#B
"
B
aB
�B
,�B
�B
(�B
A�B
TlB
[�B
:B
�cB
��B
�B�B�B.�BP�B�wB�B�ZB�yB�TB�	B��B��B��B�&B��B��B�wB��B��B�$Bv7Bh�BXBA�B[B
��B
�2B
�NB
b�B
5�B
OB	�B	�iB	��B	�TB	�KB	��B	y�B	{UB	��B	��B	��B	�NB	� B	��B	��B	��B	�;B	�1B	��B
'�B
:�B
:jB
9cB
;pB
J/B
V�B
c+B
m�B
sYB
|�B
��B
��B
�CB
��B
x�B
�!B
�BB
�B
�
B
��B
̤B
ҔB
��B
�B
�B
�B
��B
�`B
�B
��B
��B
�8B
z�B
d�B
_B
X�B
VB
U�B
S�B
XPB
[�B
b�B
aB
\4B
X�B
U�B
W�B
R�B
Q�B
Q�B
R�B
R�B
U	B
U�B
T8B
TB
VDB
]oB
^uB
]oB
[cB
\�B
WJB
S1B
R+B
P�B
N|B
OMB
NGB
NB
MuB
MuB
MAB
OB
OMB
OMB
R+B
R�B
W~B
WJB
VxB
Y�B
W�B
W�B
VDB
VxB
X�B
VDB
X�B
]�B
_�B
`�B
aB
a�B
b�B
c+B
b�B
b�B
b�B
d�B
c�B
a�B
`MB
^�B
\iB
Y�B
X�B
WJB
W~B
VDB
UrB
UrB
T�B
SfB
R�B
O�B
N�B
OB
NB
M�B
N|B
N|B
NB
M�B
N�B
M�B
M�B
L�B
L�B
MB
MB
L�B
KiB
K5B
KiB
K5B
J�B
JcB
J/B
K�B
LB
LB
LoB
K�B
K�B
K5B
L�B
JcB
JcB
JcB
J/B
I�B
H�B
H�B
HWB
H"B
F�B
F�B
F�B
EyB
E�B
D�B
D�B
D�B
D
B
ClB
D>B
ClB
ClB
B�B
BfB
A�B
CB
>�B
>�B
=�B
=B
=HB
;�B
=�B
:5B
8�B
7#B
7WB
5KB
4�B
0�B
/�B
.TB
-B
+vB
*pB
*�B
,HB
+BB
*<B
*pB
&�B
&�B
&WB
&#B
%�B
%�B
$�B
#�B
$�B
#EB
"?B
,B
HB
�B
B
�B
jB
�B
�B
�B
RB
LB
�B
zB
B
EB
�B
3B
�B
UB
�B

�B
	�B

B

IB
�B

}B
B
�B
�B
�B
�B
�B
�B
�B
'B
�B
[B

B
	�B
�B
<B
�B
�B
�B
�B
�B
�B
�B
eB
6B
�B
�B
B
�B
B
6B
�B
6B
B
qB
�B
kB
�B
6B
�B
�B
�B
qB
<B
qB
�B
�B
�B
�B
<B
<B
	�B
�B
	�B
B
B
�B
qB
	�B

B

�B

IB

�B

�B

�B

�B

}B

�B

�B
B
�B
�B
OB
�B
�B
�B
�B
�B
�B
[B
�B
�B
UB
�B
UB
OB
�B
�B
�B
�B
'B
[B
[B
[B
'B
�B
�B
aB
�B
-B
�B
�B
�B
�B
3B
gB
3B
B
9B
9B
B
B
EB
�B
�B
B
�B
�B
zB
EB
zB
�B
B
�B
�B
�B
�B
0B
dB
�B
�B
�B
�B
�B
B
B
wB
BB
B
}B
B
B
UB
�B
�B
UB
�B
�B
�B
�B
�B
 3B
�B
�B
 �B
!�B
!�B
"
B
"
B
"�B
#yB
#yB
#�B
$KB
$KB
$�B
$�B
$�B
$�B
%QB
%�B
%�B
%�B
%�B
&WB
&WB
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(dB
'�B
(dB
(/B
(/B
'�B
'�B
)�B
)B
(�B
(�B
)jB
)B
(�B
)jB
*<B
)�B
)jB
+BB
+BB
+B
+B
*�B
*�B
+vB
+vB
+�B
+�B
,HB
-�B
-NB
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
/&B
/�B
/ZB
/�B
0�B
1�B
1�B
2mB
2mB
28B
3>B
3>B
3>B
3�B
4EB
4yB
4�B
4�B
5B
5�B
6B
6�B
6B
6B
6�B
6�B
7�B
6�B
7#B
7�B
8]B
8�B
8�B
9/B
9/B
9/B
9/B
8�B
9�B
9/B
9�B
;B
;B
:�B
;�B
<�B
=B
=HB
=HB
=HB
=|B
=�B
=�B
>�B
>NB
?B
?�B
?�B
?�B
@ZB
@&B
@&B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
CB
ClB
ClB
C�B
D
B
C�B
D
B
D
B
D�B
EB
EB
EDB
EB
EDB
EyB
E�B
FB
FB
F�B
FJB
GB
F�B
GB
GQB
F�B
G�B
G�B
H"B
HWB
H�B
H�B
I]B
I�B
K B
I�B
J�B
J�B
K5B
K5B
L;B
L;B
LB
L;B
L�B
L�B
MuB
M�B
N|B
N�B
OMB
O�B
O�B
OMB
OMB
O�B
O�B
O�B
O�B
PB
PB
PB
P�B
P�B
P�B
P�B
QZB
QZB
Q%B
Q�B
Q�B
QZB
Q�B
R+B
R�B
R�B
S�B
S1B
S�B
TlB
S�B
TB
S�B
TB
S�B
T�B
VDB
VDB
VDB
WB
WJB
V�B
VxB
WB
V�B
W�B
XB
XPB
X�B
X�B
XPB
X�B
XPB
X�B
X�B
X�B
XPB
Y"B
Y"B
Y�B
Y"B
X�B
Y"B
Y"B
YVB
Y�B
Y�B
Z(B
Z�B
Z�B
Z�B
Z�B
[.B
Z�B
[.B
[�B
[cB
[�B
[�B
\iB
\iB
]�B
]:B
]oB
]oB
]�B
^B
^AB
^B
^uB
^�B
^�B
_B
_{B
_GB
_{B
_�B
_�B
`B
`B
`B
`B
`MB
`�B
`�B
`�B
aB
aB
aSB
a�B
a�B
b%B
b%B
bYB
bYB
b�B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
d1B
d1B
d1B
d�B
d�B
deB
eB
eB
e�B
f	B
f=B
f=B
f�B
gB
gB
gxB
g�B
g�B
hB
hJB
h~B
h�B
h�B
iB
iB
i�B
i�B
i�B
i�B
jVB
j�B
k(B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
mhB
m�B
m�B
m�B
nB
m�B
n�B
n�B
n�B
o@B
o@B
o�B
o�B
ouB
o�B
o@B
n�B
nnB
n�B
oB
o�B
p�B
q�B
q�B
rB
rB
rB
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
v7B
vkB
v7B
v�B
w=B
w=B
w=B
w�B
xB
xCB
xCB
xB
x�B
x�B
yIB
yIB
yB
x�B
y~B
zOB
zOB
z�B
zOB
z�B
{!B
{!B
{�B
{�B
|'B
|�B
|�B
|\B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
~3B
~3B
}�B
}�B
~3B
~�B
B
:B
:B
nB
�tB
�tB
�@B
�tB
��B
�B
�FB
��B
��B
�B
��B
�B
��B
��B
�LB
��B
��B
��B
��B
�RB
�RB
��B
��B
��B
��B
��B
��B
��B
��B
�XB
�$B
��B
��B
��B
��B
��B
�*B
�*B
�*B
�*B
�*B
�0B
�0B
�B
�6B
�B
�B
��B
��B
�B
�B
�<B
�B
�qB
��B
��B
��B
�B
�CB
�CB
��B
��B
��B
��B
�IB
��B
��B
��B
��B
��B
��B
$�B
'^B
%QB
'^B
'�B
'�B
(�B
&#B
(/B
%�B
"
B
&#B
&�B
(�B
'^B
'^B
&�B
%�B
')B
'�B
&�B
%QB
')B
'�B
&#B
%�B
&�B
&�B
(dB
%�B
'�B
(�B
&�B
$�B
'�B
0�B
%�B
%�B
')B
&#B
$�B
$�B
(dB
&�B
%�B
&�B
')B
&#B
%�B
%QB
'^B
%�B
%QB
&WB
'�B
$�B
#EB
(dB
&�B
&�B
%�B
%�B
'�B
&#B
%�B
%QB
'�B
&�B
'^B
&�B
%B
$B
&�B
')B
%�B
%B
$�B
(/B
&�B
%QB
&#B
%�B
#�B
$B
(dB
&#B
$KB
&#B
. B
+B
&WB
%QB
%�B
%�B
'^B
%B
'�B
%�B
&�B
%�B
$KB
%QB
%�B
&WB
%B
$�B
%�B
&�B
$�B
&WB
%�B
*�B
&#B
%B
%�B
%QB
&�B
&#B
%QB
$�B
&#B
&WB
%QB
$�B
%QB
&�B
%�B
$�B
$�B
%QB
&�B
%�B
$�B
%�B
&�B
&#B
$�B
%�B
&�B
&#B
$�B
%B
&�B
%�B
%QB
$�B
&WB
%�B
$�B
%�B
&WB
%QB
$�B
$�B
&�B
&WB
$�B
$�B
&�B
&#B
$�B
$�B
&#B
%�B
$�B
#�B
%�B
&#B
%�B
$�B
$�B
&WB
%�B
$�B
$�B
%�B
&WB
$KB
#�B
$�B
%�B
$�B
#yB
$�B
%�B
%B
#�B
$�B
%QB
%B
#yB
#yB
$�B
$�B
#�B
#B
#yB
$�B
#B
"
B
!�B
#EB
#EB
!B
 gB
&WB
 �B
 3B
�B
�B
�B
#EB
�B
�B
 B
�B
�B
�B
)B
�B
X�B
%�B
1gB
$�B
�B
&B
�B
�B
HB
B
B
#yB
"
B
%�B
#B
.�B
-�B
0,B
4yB
2�B
B�B
LoB
T8B
S�B
Y�B
TlB
R�B
U�B
T8B
TlB
U>B
UrB
YVB
o�B
gB
n:B
mhB
}-B
�LB
�*B
��B
�B
�pB
�|B
�QB
�B
��B
�vB
�2B
B
˞B
��B
�]B
�8B
�B �B"?B%�B�B�B0B�B�B�B�B#B4B"�B#�B#�B$B&#B'^B'�B'�B')B'�B&�B*B-B2�B3sB<vB;�B<vB=B<B;B:5B:�B;pB;;B9cB:�B=�B>�B>�B?�BF�BG�BH�BPBSfBT8BZ\B]oB_�B^AB]�B^uB_�B`MBeBhJBjVBi�Bn�Bu�B�B��B��B�0B��B��B��B��B�TB�yB��B�HB��B�B��B��B��B�}B�6B�<B�mB�BB�2B�,B�jB�vB�`B�8B�`B��B��B��B�)B�)B��B�cB��B�]B�B��B��B��B��B��B��B��B��B��B�uB�SB��B��B�/BǹB�,BB��B�`B�2B��B�2B�`B�yB��B˞B��BʗB�oB��B�QB��B�cB��B��B��B�jB��BáB�]B��B��B� B֭B��B�1B�B�%B�B�lBӚB��B�%B��B��BȋB�WB��B�BǹBȋB��B�`B��B�,B��B��B�TB�5B��B�|B��B�ZB�B�QB�|B�<B�pB��B�<B�B��B�sB�&B��B��B� B��B�,B�gB�B�`B��B��B��B� B��B��B��B��B��B��B��B�WB�jB�B��B�B��B�UB��B��B��B�B�B��B��B��B�BB�B�B��B��B�B��B��B��B��B�B��B�BB�mB�B��B�9B�tB��B�gB�-B��B�gB��B�B��B�B�CB�CB��B��B�qB��B��B��B�<B�eB��B��B�zBnB|'B|'B|�B�B��Bw�Bt_Bq�Bp�Br�Bq�BnnBk(Bf�BhJBr�Bk�B^AB[�BaSBm�Bj!B_{B]�B[cBU>BWBQ�BR`BW�BU�BOMBG�BK�BR`BG�B@ZB=HB=|B>B*pB*pB&WB)�B%B%QB*�B�B�B�B�B�BXBEB
��B
�hB
�FB
��B
ۗB
��B
�B
�1B
ΰB
��B
�B
��B
�PB
�B
�,B
��B
�,B
�WB
�B
�0B
��B
�-B
��B
��B
�*B
u1B
m�B
f�B
R�B
L�B
MuB
W�B
PSB
?�B
I�B
8�B
4yB
;B
A�B
&B
*<B
pB
0B
�B
B
	B
gB
�B
 B
�B	��B	�wB	�hB	�wB	��B	�B	��B	��B	�rB	�JB	ʗB	��B	��B	�DB	�B	ʗB	�;B	�sB	��B	�AB	�B	�/B	�)B	��B	��B	�QB	�>B	�,B	��B	��B	��B	��B	��B	�B	��B	�HB	�jB	��B	�B	�vB	�LB	�[B	��B	��B	�OB	�wB	�eB	��B	�B	��B	��B	��B	��B	�OB	�B	{!B	t�B	v7B	t�B	s�B	q�B	pFB	pB	m4B	u�B	xwB	��B	�zB	��B	�kB	��B	�LB	�$B	�$B	�<B	�IB	�-B	��B	��B	�}B	�jB	��B	��B	�B	�zB	�EB	�zB	��B	�B	�zB	�gB	�B	�B	�B	��B	�sB	�NB	�NB	��B	�|B	��B	� B	�TB	��B	��B	�B	��B	�ZB	��B	��B	�&B	��B	��B	��B	�&B	��B	�2B	�mB	�B	��B	��B	�B	�B	��B	��B	�`B	�JB	áB	��B	��B	�B	�WB	ȋB	�"B	�(B	��B	�]B	��B	ΰB	�%B	�`B	էB	�4B	��B	�!B	�!B	�B	�uB	��B	��B	��B	��B
 tB

�B
�B
 B
*�B
)B
)�B
+BB
/ZB
8�B
A`B
;B
;B
;B
;;B
<�B
9�B
:5B
8�B
;�B
<vB
:�B
:5B
8�B
9/B
9cB
9�B
9�B
7�B
7�B
7#B
6QB
9�B
A,B
CB
C8B
EG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721000056                            20230721000056AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072100005620230721000056  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072100005620230721000056QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072100005620230721000056QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               