CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-12-29T20:31:20Z creation; 2021-04-29T20:27:09Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     `  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     `  d@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     `  �x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` g   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �x   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �<   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20201229203120  20210429202818  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_156                 6810_008521_156                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�R��w�k@�R��w�k11  @�R�����@�R�����@2���@2����eV.	���eV.	��11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              
   
?u@   @B�\@�G�@��R@�  @�  A   A  A\)A+�A?\)A`  A�Q�A���A�Q�A�Q�A�  A�  A߮A�  B   B�
B(�B  B�B'�B/�B8(�B@(�BG�
BP  BX(�B_�
Bg�
BpQ�Bx  B�
B�{B�  B��B��
B�  B�(�B�{B��B��B�  B�  B�  B�{B�{B�{B�  B�  B��B��B��B��
B��B��B��
B�  B�{B�(�B�  B��
B�  B�{C {C
=C  C  C��C
  C��C��C  C{C  C�C��C
=C
=C
=C   C"  C$
=C&
=C(
=C*
=C+��C.
=C0
=C2  C3�C5�C8  C9��C;��C>
=C@  CA��CC��CF
=CG��CI��CK��CM��CP  CR  CT  CV
=CX  CY��C[��C]��C_�Ca�Cd  Ce��Cg�Ci�Cl  Cn
=Cp  Cq��Cs��Cv
=Cx
=Cz
=C{��C~  C��C�C�  C���C�  C�  C���C�C�  C���C���C�  C�  C�  C�  C���C�  C�C�C�  C�  C�C�  C���C�  C�  C�  C�  C�C�  C���C�  C���C�  C�C�
=C�  C�  C�
=C�  C�  C�C�  C�  C��C���C�  C�C�C�  C�C�  C�C�  C�  C�  C���C�C�  C�  C�  C�C�  C���C���C���C���C�  C�  C�  C���C�  C�C�C�C�  C�C�
=C�C�
=C�
=C�  C�
=C�C�  C���C���C���C�  C�C�C�  C���C�  C���C�  C�  C�C�C�  C�  C�  C�  C���C���C�C�
=C�  C�C�
=C�C���C�  C�C�  C���C���C�  C�  C���C�  C�C�C���C���C�C�C�  D �D ��D  D� D�qD}qD  D��D  D��D�D� D�D� D  D� D  D}qD	  D	��D
�D
� D
�qD}qD��D}qD�qD� D  D��D�D��D�D��D�D��D  D}qD  D� D  D��D�D��D�D� D  D}qD��D}qD  D� D�qD� D  D� D  D� D  D}qD�qD� D�D� D�qD � D!�D!� D"�D"��D#  D#��D$�D$� D%  D%��D&�D&� D'  D'� D'�qD(��D)�D)� D*D*�D+D+��D,  D,��D-�D-��D.�D.}qD.�qD/��D0  D0}qD0�qD1� D1�qD2z�D2��D3z�D3�qD4��D5�D5}qD6  D6��D6�qD7� D8�D8��D9D9� D9�qD:}qD:�qD;� D<  D<��D=  D=� D>  D>��D?  D?� D@�D@� D@��DA}qDB  DB��DC�DC��DD�DD� DE�DE��DFDF�DG  DG� DH�DH��DI�DI��DJDJ��DK�DK}qDK��DL}qDM�DM� DN�DN�DO�DO}qDP  DP��DQ  DQz�DR  DR�DSDS�DT�DT��DU�DU��DVDV� DV��DW}qDX�DX� DY  DY� DZ  DZ}qDZ�qD[}qD[�qD\��D]  D]��D^  D^� D_  D_� D`�D`�Da  Da}qDb  Db� Dc  Dc��DdDd��Dd�qDe��DfDf}qDf��Dgz�Dg�qDh}qDi  Di��Dj�Dj� Dk  Dk� Dl  Dl� Dl�qDmz�Dm�qDn� Do  Do� Dp�Dp�Dq�Dq� Dq�qDr� Ds�Ds� Ds�qDtz�Du  Du��Dv  Dv� Dv��Dwz�Dw��Dx}qDx�qDy}qDz�Dz��Dz�qD{}qD|  D|}qD|�qD}}qD~  D~}qD~�qD� D�HD�@ D�~�D�� D�  D�@ D�� D��HD�  D�>�D��HD��HD�  D�@ D�� D��HD�  D�>�D�� D��HD�  D�=qD�~�D���D���D�@ D�}qD���D���D�>�D�~�D��qD��qD�@ D�� D�� D�HD�AHD�� D�� D��D�AHD��HD�D�HD�@ D�~�D�� D�  D�@ D�~�D�� D�HD�>�D�~�D��HD�  D�@ D��HD��HD�HD�B�D�� D�� D�  D�>�D��HD��HD�HD�AHD���D��HD���D�@ D�� D�� D�  D�>�D�~�D���D�  D�@ D�~�D��qD���D�@ D�~�D��qD���D�AHD�� D���D�  D�>�D�~�D���D�  D�>�D�}qD�� D�HD�@ D���D��HD�  D�>�D�~�D�� D���D�>�D�~�D��HD��D�AHD�� D��HD�HD�>�D�}qD���D�HD�@ D�� D���D���D�>�D�~�D�� D���D�>�D�� D�� D�  D�B�D�� D�� D��D�B�D��HD�D��D�AHD�� D�� D�  D�@ D���D�D���D�>�D�� D���D��qD�@ D��HD��HD�  D�>�D�� D��HD�HD�AHD���D��HD���D�@ D�� D�� D�  D�@ D�� D�� D���D�=qD�~�D��HD�  D�@ D���D�D���D�AHD���D��HD�HD�C�D�� D��)D���D�@ D���D���D�HD�@ D�� D��HD�HD�AHD��HD�� D�  D�>�D�}qD�� D�  D�>�D��HD�� D�  D�AHD�~�D��qD���D�>�D�}qD���D�  D�@ D�� D�� D�HD�AHD���D��HD�HD�@ D�� D���D�  D�@ D�~�D�� D�HD�AHD�~�D���D�  D�@ D�� D���D���D�@ D�� D�� D�  D�AHD��HD��HD�  D�AHD�� D���D�  D�>�D�� D�� D�  D�@ D�~�D���D��qD�=qD�~�D��HD�HD�@ DHD��HD�  D�@ DÁHD�� D���D�>�DĀ D�� D�HD�>�D�~�Dž�D�  D�@ DƁHD�D�HD�@ Dǀ D�� D�HD�AHDȀ DȾ�D���D�=qDɀ D�� D�  D�AHDʁHD�� D�  D�@ DˁHD��HD�HD�AHD̀ D�� D���D�@ D�~�D;�D���D�@ D΀ D�� D�  D�@ DρHDϾ�D���D�@ D�~�D��HD��D�AHDр D��HD�HD�@ DҁHD��HD�  D�@ DӁHD��HD�  D�@ DԀ D��HD�HD�@ D�~�D��HD�  D�=qDր D��HD�  D�=qD�~�D�� D���D�>�D؁HD�� D��qD�=qD�}qDپ�D�  D�AHDځHD�D�  D�@ D�~�D�� D�HD�@ D܁HD��HD�HD�@ D݁HD�� D�  D�AHDހ D޾�D�HD�B�D߀ D߾�D���D�>�D�� D��HD��D�AHD� D�� D�  D�>�D�~�D⾸D�  D�AHDわD�D�HD�AHD� D�� D�  D�@ D�~�D�qD�  D�AHD�~�D澸D���D�=qD�~�D羸D���D�@ D�~�D�qD�HD�AHD� D龸D�  D�@ D�HD�D�HD�>�D�}qD뾸D�  D�AHD�HD��HD��D�B�D킏D��HD��D�@ D�~�D��HD�  D�>�D�~�D�� D���D�>�D�� D��HD�  D�=qD�}qD�� D�HD�B�D� D�D���D�@ D�D�D�HD�@ D� D�� D�HD�AHD��HD�� D���D�AHD��HD���D�  D�>�D�~�D���D��qD�@ D��HD�� D���D�AHD�}q?\)?L��?�z�?�33?�ff?��H@z�@#�
@5@L��@W
=@k�@}p�@�ff@���@�Q�@��R@��@���@���@\@���@��@��H@�  @�@��@���AG�A�A��A�RAG�AffA=qA{A#33A&ffA)��A0  A2�\A6ffA<(�A?\)ADz�AG�AJ�HAP��AS33AW
=A]p�A_\)Ac33Ah��Ak�Ap  Au�Aw�A|(�A�Q�A���A��A�{A�\)A�=qA��A�p�A�  A�G�A�33A�p�A�\)A���A��A��A�
=A���A�33A�p�A�  A���A���A��RA�Q�A��A�p�A�
=A�=qA�(�A�{A���A\A�z�AǮAə�A˅A�{AУ�A�=qA��A�\)Aٙ�A�z�A�ffA�G�A�A��A�Q�A�\A�(�A�
=A�G�A��HA�A�\)A���A�z�A�B   BG�B�B33BQ�B��BffB\)BQ�B	��B
�RB\)B��B�B�RB  B��B�B33B  B��B=qB�HB(�BG�B�B
=B(�B��B{B
=B�B ��B!��B"=qB#�B$Q�B$��B&{B'
=B'�B(��B)B*ffB+\)B,z�B-�B.=qB/33B/�B0��B1�B2�\B3�B4��B5��B6ffB7�B8Q�B9�B:ffB:�HB<  B=�B=��B>�RB?�
B@z�BAG�BB�\BC
=BD(�BE�BEBF�HBG�
BH��BI�BJ�\BK\)BL��BM��BN=qBO�BP(�BQ�BRffBR�HBT  BUG�BU�BV�RBX  BX��BYp�BZ�RB\  B\z�B]B^�RB_�B`��Ba�Bb�\Bc�Bd��Be��BfffBg�
Bhz�BiBj�HBk�Bl��Bm�Bn�\Bo�
Bp��Bqp�Br�HBs�Bt��Bu�Bv�RBw�Bx��By�Bz�RB|(�B|��B}B33B�{B��\B�33B���B�=qB���B�33B��B�ffB��HB�G�B�  B�ffB��HB��B�{B�z�B�33B���B�{B��RB�33B���B�Q�B���B�\)B�  B��RB�
=B�B�ffB���B�G�B�{B��\B��B�B�=qB���B��B��B�ffB��B���B�  B���B�G�B��B�=qB���B�\)B��
B�z�B��B��B�{B���B��B��B�ffB��HB�G�B�  B�z�B���B��B�{B�z�B�33B���B�  B���B�G�B���B�(�B���B�33B���B�=qB���B��B�B�Q�B��RB��B��
B�ffB��RB�G�B�  B�Q�B���B��B�  B�Q�B���B���B�{B�ffB���B��B�  B�Q�B���B�p�B��
B�=qB��HB�G�B��B�Q�B���B��B��B�=qB���B�
=B���B�(�B��\B���B��B�  B�Q�B���B�p�B�B�Q�B���B�G�BîB�=qB���B�33BŅB�{Bƣ�B�
=B�p�B�  Bȏ\B��HB�p�B�  Bʏ\B��HB�G�B��B�z�B���B�G�B��
B�Q�BΣ�B��BϮB�{B�ffB��HB�p�B�  B�=qBң�B�33B�B�  B�ffB��HB�p�B�  B�=qB֣�B�33B�B�(�B؏\B��Bٙ�B��B�Q�B��HB�p�B��B�=qBܸRB�33B��
B�Q�B޸RB�G�B��B�z�B��HB�p�B�{B��B�
=B�B�ffB���B�33B�  B�z�B���B�B�(�B��B��B陚B�Q�B�RB�33B��
B�z�B�
=B�p�B��B��B�G�B�B�{B���B�p�B�  B�ffB���B�B�=qB���B�G�B�  B�z�B��HB��B�Q�B���B�33B�  B���B�
=B���B�Q�B���B�\)B��B��RB�\)B��
C 33C z�C �
C�C\)C��C  C\)C��C�
C�Cz�C�
C�CffC�C{CffC�C�CG�C��C�C33Cz�C�HC33CffC�RC	{C	ffC	�RC	��C
=qC
��C
��C(�Cp�CC�C\)C��C�HCG�C�\CC
=C\)C�RC
=CG�C�\C�CG�Cz�CC�Cp�C�C�CG�C��C�HC{Cp�CC��CG�C��C�C�CffCC  C=qC�\C�HC(�CffC�RC  C=qCz�C�
C{CG�C��C�HC�C\)C�C�HC�Cz�C�C�HCG�C�\C�RC{CffC��C�
C33Cz�C�C  CQ�C��C��C 
=C \)C �C �
C!33C!p�C!��C!�HC"=qC"z�C"�C"�C#=qC#�C#��C$  C$Q�C$�C$�
C%{C%ffC%C%�C&33C&��C&��C'
=C'\)C'�C'�C(�C(z�C(��C(��C)=qC)�\C)�
C*
=C*Q�C*�C*�C+�C+p�C+��C,
=C,G�C,��C,�C-=qC-p�C-�C.
=C.\)C.��C.��C/{C/p�C/C/�C033C0�\C0�
C1{C1\)C1C2  C2=qC2��C2�C3�C3\)C3C4
=C4=qC4��C4�C5(�C5ffC5C6{C6G�C6�C6�
C733C7ffC7�C7�C8=qC8��C8�
C9{C9ffC9�RC:  C:=qC:z�C:�
C;33C;z�C;��C;��C<Q�C<�C<C=�C=ffC=��C=�C>G�C>�\C>��C?
=C?\)C?�RC?��C@=qC@z�C@CA{CAp�CA�RCA��CB33CBz�CB�
CC�CC\)CC��CC�CDG�CD�CDCE(�CEz�CE�RCE��CFQ�CF��CF�HCG�CGz�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                              ?u@   @B�\@�G�@��R@�  @�  A   A  A\)A+�A?\)A`  A�Q�A���A�Q�A�Q�A�  A�  A߮A�  B   B�
B(�B  B�B'�B/�B8(�B@(�BG�
BP  BX(�B_�
Bg�
BpQ�Bx  B�
B�{B�  B��B��
B�  B�(�B�{B��B��B�  B�  B�  B�{B�{B�{B�  B�  B��B��B��B��
B��B��B��
B�  B�{B�(�B�  B��
B�  B�{C {C
=C  C  C��C
  C��C��C  C{C  C�C��C
=C
=C
=C   C"  C$
=C&
=C(
=C*
=C+��C.
=C0
=C2  C3�C5�C8  C9��C;��C>
=C@  CA��CC��CF
=CG��CI��CK��CM��CP  CR  CT  CV
=CX  CY��C[��C]��C_�Ca�Cd  Ce��Cg�Ci�Cl  Cn
=Cp  Cq��Cs��Cv
=Cx
=Cz
=C{��C~  C��C�C�  C���C�  C�  C���C�C�  C���C���C�  C�  C�  C�  C���C�  C�C�C�  C�  C�C�  C���C�  C�  C�  C�  C�C�  C���C�  C���C�  C�C�
=C�  C�  C�
=C�  C�  C�C�  C�  C��C���C�  C�C�C�  C�C�  C�C�  C�  C�  C���C�C�  C�  C�  C�C�  C���C���C���C���C�  C�  C�  C���C�  C�C�C�C�  C�C�
=C�C�
=C�
=C�  C�
=C�C�  C���C���C���C�  C�C�C�  C���C�  C���C�  C�  C�C�C�  C�  C�  C�  C���C���C�C�
=C�  C�C�
=C�C���C�  C�C�  C���C���C�  C�  C���C�  C�C�C���C���C�C�C�  D �D ��D  D� D�qD}qD  D��D  D��D�D� D�D� D  D� D  D}qD	  D	��D
�D
� D
�qD}qD��D}qD�qD� D  D��D�D��D�D��D�D��D  D}qD  D� D  D��D�D��D�D� D  D}qD��D}qD  D� D�qD� D  D� D  D� D  D}qD�qD� D�D� D�qD � D!�D!� D"�D"��D#  D#��D$�D$� D%  D%��D&�D&� D'  D'� D'�qD(��D)�D)� D*D*�D+D+��D,  D,��D-�D-��D.�D.}qD.�qD/��D0  D0}qD0�qD1� D1�qD2z�D2��D3z�D3�qD4��D5�D5}qD6  D6��D6�qD7� D8�D8��D9D9� D9�qD:}qD:�qD;� D<  D<��D=  D=� D>  D>��D?  D?� D@�D@� D@��DA}qDB  DB��DC�DC��DD�DD� DE�DE��DFDF�DG  DG� DH�DH��DI�DI��DJDJ��DK�DK}qDK��DL}qDM�DM� DN�DN�DO�DO}qDP  DP��DQ  DQz�DR  DR�DSDS�DT�DT��DU�DU��DVDV� DV��DW}qDX�DX� DY  DY� DZ  DZ}qDZ�qD[}qD[�qD\��D]  D]��D^  D^� D_  D_� D`�D`�Da  Da}qDb  Db� Dc  Dc��DdDd��Dd�qDe��DfDf}qDf��Dgz�Dg�qDh}qDi  Di��Dj�Dj� Dk  Dk� Dl  Dl� Dl�qDmz�Dm�qDn� Do  Do� Dp�Dp�Dq�Dq� Dq�qDr� Ds�Ds� Ds�qDtz�Du  Du��Dv  Dv� Dv��Dwz�Dw��Dx}qDx�qDy}qDz�Dz��Dz�qD{}qD|  D|}qD|�qD}}qD~  D~}qD~�qD� D�HD�@ D�~�D�� D�  D�@ D�� D��HD�  D�>�D��HD��HD�  D�@ D�� D��HD�  D�>�D�� D��HD�  D�=qD�~�D���D���D�@ D�}qD���D���D�>�D�~�D��qD��qD�@ D�� D�� D�HD�AHD�� D�� D��D�AHD��HD�D�HD�@ D�~�D�� D�  D�@ D�~�D�� D�HD�>�D�~�D��HD�  D�@ D��HD��HD�HD�B�D�� D�� D�  D�>�D��HD��HD�HD�AHD���D��HD���D�@ D�� D�� D�  D�>�D�~�D���D�  D�@ D�~�D��qD���D�@ D�~�D��qD���D�AHD�� D���D�  D�>�D�~�D���D�  D�>�D�}qD�� D�HD�@ D���D��HD�  D�>�D�~�D�� D���D�>�D�~�D��HD��D�AHD�� D��HD�HD�>�D�}qD���D�HD�@ D�� D���D���D�>�D�~�D�� D���D�>�D�� D�� D�  D�B�D�� D�� D��D�B�D��HD�D��D�AHD�� D�� D�  D�@ D���D�D���D�>�D�� D���D��qD�@ D��HD��HD�  D�>�D�� D��HD�HD�AHD���D��HD���D�@ D�� D�� D�  D�@ D�� D�� D���D�=qD�~�D��HD�  D�@ D���D�D���D�AHD���D��HD�HD�C�D�� D��)D���D�@ D���D���D�HD�@ D�� D��HD�HD�AHD��HD�� D�  D�>�D�}qD�� D�  D�>�D��HD�� D�  D�AHD�~�D��qD���D�>�D�}qD���D�  D�@ D�� D�� D�HD�AHD���D��HD�HD�@ D�� D���D�  D�@ D�~�D�� D�HD�AHD�~�D���D�  D�@ D�� D���D���D�@ D�� D�� D�  D�AHD��HD��HD�  D�AHD�� D���D�  D�>�D�� D�� D�  D�@ D�~�D���D��qD�=qD�~�D��HD�HD�@ DHD��HD�  D�@ DÁHD�� D���D�>�DĀ D�� D�HD�>�D�~�Dž�D�  D�@ DƁHD�D�HD�@ Dǀ D�� D�HD�AHDȀ DȾ�D���D�=qDɀ D�� D�  D�AHDʁHD�� D�  D�@ DˁHD��HD�HD�AHD̀ D�� D���D�@ D�~�D;�D���D�@ D΀ D�� D�  D�@ DρHDϾ�D���D�@ D�~�D��HD��D�AHDр D��HD�HD�@ DҁHD��HD�  D�@ DӁHD��HD�  D�@ DԀ D��HD�HD�@ D�~�D��HD�  D�=qDր D��HD�  D�=qD�~�D�� D���D�>�D؁HD�� D��qD�=qD�}qDپ�D�  D�AHDځHD�D�  D�@ D�~�D�� D�HD�@ D܁HD��HD�HD�@ D݁HD�� D�  D�AHDހ D޾�D�HD�B�D߀ D߾�D���D�>�D�� D��HD��D�AHD� D�� D�  D�>�D�~�D⾸D�  D�AHDわD�D�HD�AHD� D�� D�  D�@ D�~�D�qD�  D�AHD�~�D澸D���D�=qD�~�D羸D���D�@ D�~�D�qD�HD�AHD� D龸D�  D�@ D�HD�D�HD�>�D�}qD뾸D�  D�AHD�HD��HD��D�B�D킏D��HD��D�@ D�~�D��HD�  D�>�D�~�D�� D���D�>�D�� D��HD�  D�=qD�}qD�� D�HD�B�D� D�D���D�@ D�D�D�HD�@ D� D�� D�HD�AHD��HD�� D���D�AHD��HD���D�  D�>�D�~�D���D��qD�@ D��HD�� D���D�AHG�O�?\)?L��?�z�?�33?�ff?��H@z�@#�
@5@L��@W
=@k�@}p�@�ff@���@�Q�@��R@��@���@���@\@���@��@��H@�  @�@��@���AG�A�A��A�RAG�AffA=qA{A#33A&ffA)��A0  A2�\A6ffA<(�A?\)ADz�AG�AJ�HAP��AS33AW
=A]p�A_\)Ac33Ah��Ak�Ap  Au�Aw�A|(�A�Q�A���A��A�{A�\)A�=qA��A�p�A�  A�G�A�33A�p�A�\)A���A��A��A�
=A���A�33A�p�A�  A���A���A��RA�Q�A��A�p�A�
=A�=qA�(�A�{A���A\A�z�AǮAə�A˅A�{AУ�A�=qA��A�\)Aٙ�A�z�A�ffA�G�A�A��A�Q�A�\A�(�A�
=A�G�A��HA�A�\)A���A�z�A�B   BG�B�B33BQ�B��BffB\)BQ�B	��B
�RB\)B��B�B�RB  B��B�B33B  B��B=qB�HB(�BG�B�B
=B(�B��B{B
=B�B ��B!��B"=qB#�B$Q�B$��B&{B'
=B'�B(��B)B*ffB+\)B,z�B-�B.=qB/33B/�B0��B1�B2�\B3�B4��B5��B6ffB7�B8Q�B9�B:ffB:�HB<  B=�B=��B>�RB?�
B@z�BAG�BB�\BC
=BD(�BE�BEBF�HBG�
BH��BI�BJ�\BK\)BL��BM��BN=qBO�BP(�BQ�BRffBR�HBT  BUG�BU�BV�RBX  BX��BYp�BZ�RB\  B\z�B]B^�RB_�B`��Ba�Bb�\Bc�Bd��Be��BfffBg�
Bhz�BiBj�HBk�Bl��Bm�Bn�\Bo�
Bp��Bqp�Br�HBs�Bt��Bu�Bv�RBw�Bx��By�Bz�RB|(�B|��B}B33B�{B��\B�33B���B�=qB���B�33B��B�ffB��HB�G�B�  B�ffB��HB��B�{B�z�B�33B���B�{B��RB�33B���B�Q�B���B�\)B�  B��RB�
=B�B�ffB���B�G�B�{B��\B��B�B�=qB���B��B��B�ffB��B���B�  B���B�G�B��B�=qB���B�\)B��
B�z�B��B��B�{B���B��B��B�ffB��HB�G�B�  B�z�B���B��B�{B�z�B�33B���B�  B���B�G�B���B�(�B���B�33B���B�=qB���B��B�B�Q�B��RB��B��
B�ffB��RB�G�B�  B�Q�B���B��B�  B�Q�B���B���B�{B�ffB���B��B�  B�Q�B���B�p�B��
B�=qB��HB�G�B��B�Q�B���B��B��B�=qB���B�
=B���B�(�B��\B���B��B�  B�Q�B���B�p�B�B�Q�B���B�G�BîB�=qB���B�33BŅB�{Bƣ�B�
=B�p�B�  Bȏ\B��HB�p�B�  Bʏ\B��HB�G�B��B�z�B���B�G�B��
B�Q�BΣ�B��BϮB�{B�ffB��HB�p�B�  B�=qBң�B�33B�B�  B�ffB��HB�p�B�  B�=qB֣�B�33B�B�(�B؏\B��Bٙ�B��B�Q�B��HB�p�B��B�=qBܸRB�33B��
B�Q�B޸RB�G�B��B�z�B��HB�p�B�{B��B�
=B�B�ffB���B�33B�  B�z�B���B�B�(�B��B��B陚B�Q�B�RB�33B��
B�z�B�
=B�p�B��B��B�G�B�B�{B���B�p�B�  B�ffB���B�B�=qB���B�G�B�  B�z�B��HB��B�Q�B���B�33B�  B���B�
=B���B�Q�B���B�\)B��B��RB�\)B��
C 33C z�C �
C�C\)C��C  C\)C��C�
C�Cz�C�
C�CffC�C{CffC�C�CG�C��C�C33Cz�C�HC33CffC�RC	{C	ffC	�RC	��C
=qC
��C
��C(�Cp�CC�C\)C��C�HCG�C�\CC
=C\)C�RC
=CG�C�\C�CG�Cz�CC�Cp�C�C�CG�C��C�HC{Cp�CC��CG�C��C�C�CffCC  C=qC�\C�HC(�CffC�RC  C=qCz�C�
C{CG�C��C�HC�C\)C�C�HC�Cz�C�C�HCG�C�\C�RC{CffC��C�
C33Cz�C�C  CQ�C��C��C 
=C \)C �C �
C!33C!p�C!��C!�HC"=qC"z�C"�C"�C#=qC#�C#��C$  C$Q�C$�C$�
C%{C%ffC%C%�C&33C&��C&��C'
=C'\)C'�C'�C(�C(z�C(��C(��C)=qC)�\C)�
C*
=C*Q�C*�C*�C+�C+p�C+��C,
=C,G�C,��C,�C-=qC-p�C-�C.
=C.\)C.��C.��C/{C/p�C/C/�C033C0�\C0�
C1{C1\)C1C2  C2=qC2��C2�C3�C3\)C3C4
=C4=qC4��C4�C5(�C5ffC5C6{C6G�C6�C6�
C733C7ffC7�C7�C8=qC8��C8�
C9{C9ffC9�RC:  C:=qC:z�C:�
C;33C;z�C;��C;��C<Q�C<�C<C=�C=ffC=��C=�C>G�C>�\C>��C?
=C?\)C?�RC?��C@=qC@z�C@CA{CAp�CA�RCA��CB33CBz�CB�
CC�CC\)CC��CC�CDG�CD�CDCE(�CEz�CE�RCE��CFQ�CF��CF�HCG�CGz�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                              @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�
=A�bA�VA�bA�bA�bA�oA�bA�oA�{A�oA�{A�{A�{A��A��A��A��A��A�{A�{A��A��A��A��A� �A� �A�"�A�(�A�7LA�G�A�^5A�jA�jA�ffA�bNA�`BA�K�A�A�A�?}A�A�A�E�A�1'A�-A�/A�-A�-A���A�O�A՝�A�AԴ9A�XAӬAғuAѰ!A��mA�^5AϸRAΩ�A̺^A�bA�-AȑhA�^5A���A�XA�;dA�jA��
A���A��A��A��A�v�A�\)A���A�hsA�jA��A�E�A�ZA��+A�JA�Q�A�^5A���A�z�A�VA��;A�v�A�jA���A�JA�\)A���A�?}A�p�A�ȴA��A�VA�l�A��A���A��A���A��A���A�ĜA�"�A��
A�?}A��A�S�A�ȴA�XA��A�`BA�ffA��mA��A���A�`BA�\)A��7A��A�{A�-A��DA�%A�A{l�Aw��At��Ar�9Aqt�Aq�Ao�Am�PAkAj�Af�Ad��Ac�-AbbNAa7LA\�AZjATĜANM�AL��AK?}AE�AB�RABA�AB �AA�AAƨAAhsA@ �A;"�A8�A7��A7�A6�+A61'A5��A4�`A4E�A3%A2�jA2�9A2��A1�wA/��A/�A/O�A/�A/%A.��A+�A*9XA)l�A'�-A%��A$$�A!��A!VA ��A =qA�;A�9AA\)A�uAI�A�wA��A{A�uA�AhsA �A�A�TA��Av�A��A�A7LA~�A �A�AA�PA+A�/Av�A�AK�A�A
�`A
ZA
$�A	��A	O�A	7LA	&�AĜAr�A�A\)A5?A`BAO�A7LAZAhsA;dA�`Az�A��A�A ��A   @�Ĝ@��^@��7@�@�j@�E�@��@���@�ȴ@�v�@��T@�@�O�@�V@���@�Ĝ@��`@���@�r�@��@�^5@�M�@�@�Q�@�V@�X@�ƨ@��#@���@�t�@�v�@�-@��#@�hs@���@���@�bN@��
@۝�@ۥ�@۶F@�S�@���@�|�@�E�@ڗ�@أ�@�X@ԋD@�j@�I�@��m@��@�J@с@�G�@�&�@��@�&�@�/@�/@�&�@��/@�I�@мj@���@�1@�  @���@�Z@�9X@�t�@���@Ͳ-@�I�@�E�@ɡ�@�-@�@�V@��@�^5@�=q@�I�@��;@�(�@ǥ�@���@���@�%@�hs@ř�@�X@ģ�@�  @öF@ÍP@�dZ@°!@+@�ff@�=q@�@���@��@�r�@��;@�\)@�+@�
=@��!@�^5@�$�@���@�hs@�7L@��@�I�@��@�  @���@��@���@���@�ff@��@���@�O�@�?}@��@��j@�1@��@�C�@��H@�~�@�{@��#@��^@���@��7@�O�@�Z@��@��
@��w@���@���@�~�@���@��@��u@��@�Q�@� �@�1@�ƨ@�C�@��@�
=@��@��y@��\@�=q@��#@��@�O�@��/@�z�@�  @�|�@��@��R@�ff@��@���@�(�@�b@�1@�  @��@��@�l�@���@��@���@�G�@�V@�Ĝ@��u@�Z@� �@���@��@�C�@�o@�@���@��@��R@��\@�n�@�E�@�{@��h@�7L@���@��D@��@���@���@��+@�=q@��@��@�@�`B@���@���@���@�r�@� �@��m@���@�\)@���@���@��+@�=q@��@���@�p�@�O�@�?}@��@���@��`@��@�bN@�b@�\)@���@�ȴ@���@���@�ff@�5?@���@��7@�X@�%@��j@�r�@� �@��
@�\)@�33@�"�@�
=@���@�M�@�{@�@��T@��@�X@��@��@��j@�z�@�r�@�bN@�I�@�(�@� �@���@���@�S�@�o@��H@��\@�V@�J@��#@�x�@�G�@�V@��9@�z�@�Z@�9X@��
@�ƨ@���@�\)@�S�@�S�@�K�@�;d@�33@�+@�ȴ@�ff@�$�@��@�/@�r�@�  @��F@���@���@���@��P@�t�@�l�@�dZ@�\)@�\)@�S�@�C�@�C�@�;d@�o@�v�@��@���@���@�`B@�/@��@��u@�I�@��m@���@�l�@��@�ff@���@��7@�`B@�/@��@�bN@�Q�@�A�@�A�@�9X@�1'@� �@�b@�@��@�P@;d@~��@~��@~E�@}��@}O�@|��@|�@{��@{t�@{@z^5@z�@y�#@y7L@x��@x��@x�@xbN@x �@w;d@v��@v��@vv�@u��@u?}@t�/@t�D@t�D@tj@tI�@t9X@s��@s��@sdZ@so@r�\@q��@qhs@qG�@qG�@qG�@q�@q�@p��@pr�@o�@p  @o|�@n�y@nE�@m�@m/@m�@l��@l�@l��@k�
@kdZ@k33@k"�@k@j�H@j~�@j^5@j=q@j=q@j-@i�^@ix�@ix�@ix�@iG�@i7L@i�@hr�@h1'@hb@hb@g�@g�@f�@f�R@fE�@e�T@e�h@d��@d��@d1@ct�@co@b��@b^5@a��@`��@` �@_��@_|�@_+@^ȴ@^v�@^$�@]��@]O�@]/@\�@\��@\(�@[�
@[t�@["�@[o@[o@[o@[o@Z�@Z~�@Y�^@YG�@XĜ@X1'@W�w@W
=@V�+@VV@VE�@U�-@Up�@U`B@U?}@U�@T�/@TI�@T�@S�m@S��@SC�@S@R�!@Rn�@R-@Q��@QX@Q&�@PĜ@O�@O��@Ol�@O+@N�y@Nv�@N@M�h@M`B@L��@L�@Lj@L1@K�F@KC�@J�@J��@J��@J��@J��@J~�@J^5@J=q@JJ@I��@I�@H��@HbN@H �@G�;@G�@G�@Fȴ@F��@FV@FE�@F$�@F@E�T@E@Ep�@D��@D�D@D(�@C�
@Ct�@B��@BM�@B�@A�@A�^@A&�@@��@@��@@bN@?�P@?K�@?;d@?�@?
=@>�R@>ff@=��@=p�@=?}@<��@<�j@<�@<��@;ƨ@:�H@:�!@:n�@:M�@9��@9X@9%@8��@8�`@8�@8Q�@7�;@6�y@6V@5�@5p�@5/@4�@41@3�F@3�@3"�@2��@2^5@1x�@0��@0b@0  @0  @/�@/�@/�@/l�@/K�@/;d@/�@.��@.ff@.$�@.@-�@-@-�@-V@,�@,I�@+�F@+dZ@+S�@+33@*�H@*�@)�@)�^@)x�@)G�@(��@(�u@(r�@(Q�@(b@'�w@'��@'l�@'l�@'\)@'
=@&��@&ff@&5?@&{@%��@%�h@%O�@%O�@%/@%�@$�/@$�j@$�D@$�D@$z�@$j@$9X@$1@#��@#ƨ@#��@#t�@#C�@#@"�H@"��@"��@"n�@"=q@!��@!�^@!x�@!G�@!&�@ �`@ bN@ A�@ b@��@l�@+@�@��@v�@ff@$�@@�@��@�@`B@�@�@�/@�@�D@Z@��@33@o@�@�H@��@��@��@��@�\@~�@M�@-@�@��@��@hs@G�@%@�9@Q�@ �@b@b@b@  @�@�;@�@|�@
=@ȴ@��@�+@�+@V@�-@?}@�@V@�@�@j@�@��@��@�m@�m@�m@ƨ@t�@"�@@�@�H@�\@~�@M�@J@�@�^@G�@7L@�@��@��@r�A�1A�
=A�JA�JA�oA�JA�oA�bA�VA�oA�JA�VA�bA�VA�oA�oA�VA�oA�VA�bA�{A�VA�oA�{A�VA�{A�oA�VA�{A�oA�bA�{A�VA�{A�oA�oA��A�{A�bA��A�bA�{A��A�oA��A�{A�oA��A�{A�oA��A�oA�{A��A�{A�{A��A�oA�{A��A�{A�{A��A�oA��A�{A��A��A�{A�{A��A��A�{A��A��A��A��A��A��A��A�{A��A��A�{A��A��A�{A��A��A�oA��A��A�{A��A��A��A��A��A��A��A�{A��A��A�oA��A�{A�bA�{A�oA�oA��A��A�{A��A�{A��A��A�{A��A��A�bA�{A��A�{A��A�{A�oA��A��A��A��A��A��A��A��A��A� �A��A��A�"�A��A��A�"�A��A��A� �A��A� �A� �A��A�"�A� �A��A�"�A� �A��A� �A�"�A��A�$�A�"�A��A�"�A�$�A��A�"�A�"�A��A�"�A�(�A�$�A�&�A�+A�(�A�$�A�+A�&�A�&�A�+A�(�A�+A�-A�+A�33A�=qA�9XA�=qA�A�A�=qA�?}A�C�A�?}A�I�A�E�A�G�A�M�A�M�A�VA�^5A�^5A�XA�^5A�ZA�\)A�dZA�`BA�ffA�jA�hsA�ffA�jA�jA�ffA�jA�l�A�ffA�l�A�l�A�hsA�jA�l�A�hsA�hsA�jA�hsA�hsA�jA�dZA�hsA�hsA�bNA�ffA�ffA�`BA�dZA�ffA�\)A�bNA�bNA�bNA�hsA�dZA�bNA�dZA�\)A�ZA�ZA�VA�O�A�Q�A�K�A�E�A�G�A�A�A�A�A�C�A�?}A�=qA�C�A�?}A�?}A�E�A�A�A�?}A�=qA�?}A�=qA�C�A�?}A�;dA�C�A�A�A�?}A�C�A�E�A�A�A�C�A�G�A�C�A�E�A�G�A�C�A�A�A�A�A�7LA�-A�/A�+A�+A�1'A�-A�+A�/A�-A�+A�/A�/A�+A�-A�1'A�/A�+A�-A�1'A�-A�-A�/A�+A�+A�1'A�+A�(�A�/A�+A�-A�1'A�-A�+A�/A�+A�&�A�"�A��A��A��mA��HA���A־wAְ!A֟�Aև+A�`BA�E�A�"�A��A��A��/A�ȴAհ!Aհ!Aթ�A�l�A�ffA�33A�+A��A�JA�
=A���A��A��mA��;A���A���A�ȴAԸRAԮAԬAԙ�AԑhAԉ7AԁA�v�A�jA�bNA�K�A�7LA�{A���A��;A���A���AӶFAӝ�Aӕ�A�~�A��A�&�A�{A���AҍPA�ffA�G�A�/A��A�
=A��A���AѼjAѰ!Aћ�AсA�dZA�A�A�(�A�{A���A��mA���AоwAУ�AЏ\A�x�A�hsA�dZA�ffA�`BA�S�A�S�A�M�A�9XA�5?A��A�
=A���A��TAϡ�A��A��A��#A��
A���AΧ�AΧ�AΡ�AΝ�AΛ�A΋DA�`BA��A͕�A�A̗�A�z�A�(�A���A�;dA�Aʇ+A�1'A�A��
Aɏ\A�p�A�`BA�^5A�VA�G�A�1'A�bA��A���Aȣ�Aȝ�Aș�Aȕ�AȋDAȅA�x�A�hsA�C�A�$�A�|�A�{A��
A�dZA�&�A�{A�bA�1A���A���A���AŸRAŰ!Ať�Aš�AŁA�bA�Aģ�AąA�`BA�`BA�XA�oA��#A��
Aã�Aß�AÇ+A�l�A�M�A�(�A�"�A��A���A��`A�A®A£�AhA�r�A�VA�A�A�A���A��7A�ffA�E�A�1'A��A���A��A���A�ĜA��FA���A���A���A��\A��7A��7A��DA��+A��A�|�A�|�A�z�A�v�A�t�A�r�A�p�A�t�A�ffA�dZA�`BA�dZA�`BA�VA�/A�-A� �A��A�VA��A�~�A�A�n�A�hsA�jA�bNA�I�A�;dA��`A��A�z�A�n�A�XA�/A� �A�A���A���A��A��;A��#A���A���A���A�r�A��A���A��A��`A���A��+A�l�A�^5A�?}A�$�A�VA���A��
A���A���A�|�A�dZA�E�A�oA�ĜA���A�ZA�/A�JA��;A�JA���A��wA��DA�ZA�+A�$�A��A��A�t�A�|�A�z�A�ffA�=qA�;dA�$�A��A��A��/A��/A���A�ƨA�t�A�`BA�  A��wA�p�A���A�oA�\)A�&�A���A���A�t�A�O�A�;dA�"�A�1A���A���A��A��TA��#A��A�ȴA���A��RA��A��7A�ffA�XA�Q�A��A���A���A��!A��A�n�A��A��A�ZA�&�A��A��hA�+A���A��A��HA���A�|�A�%A���A�~�A�`BA�/A��/A�bNA�/A�oA�  A���A��`A�ȴA���A��uA��7A��+A��7A��A�p�A�ffA�5?A�bA�  A��A��A��
A�A��-A�O�A��A��9A�hsA�-A���A��
A��9A�l�A���A���A���A��\A�|�A�z�A�t�A�p�A�p�A�ffA�C�A��A��mA���A��A��DA��A��A�v�A�dZA�XA�K�A�9XA�$�A�A��yA���A��DA�r�A�`BA�G�A�"�A�JA��HA��-A�|�A�S�A�(�A�JA��A���A��-A��A�O�A�&�A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                              A�
=A�bA�VA�bA�bA�bA�oA�bA�oA�{A�oA�{A�{A�{A��A��A��A��A��A�{A�{A��A��A��A��A� �A� �A�"�A�(�A�7LA�G�A�^5A�jA�jA�ffA�bNA�`BA�K�A�A�A�?}A�A�A�E�A�1'A�-A�/A�-A�-A���A�O�A՝�A�AԴ9A�XAӬAғuAѰ!A��mA�^5AϸRAΩ�A̺^A�bA�-AȑhA�^5A���A�XA�;dA�jA��
A���A��A��A��A�v�A�\)A���A�hsA�jA��A�E�A�ZA��+A�JA�Q�A�^5A���A�z�A�VA��;A�v�A�jA���A�JA�\)A���A�?}A�p�A�ȴA��A�VA�l�A��A���A��A���A��A���A�ĜA�"�A��
A�?}A��A�S�A�ȴA�XA��A�`BA�ffA��mA��A���A�`BA�\)A��7A��A�{A�-A��DA�%A�A{l�Aw��At��Ar�9Aqt�Aq�Ao�Am�PAkAj�Af�Ad��Ac�-AbbNAa7LA\�AZjATĜANM�AL��AK?}AE�AB�RABA�AB �AA�AAƨAAhsA@ �A;"�A8�A7��A7�A6�+A61'A5��A4�`A4E�A3%A2�jA2�9A2��A1�wA/��A/�A/O�A/�A/%A.��A+�A*9XA)l�A'�-A%��A$$�A!��A!VA ��A =qA�;A�9AA\)A�uAI�A�wA��A{A�uA�AhsA �A�A�TA��Av�A��A�A7LA~�A �A�AA�PA+A�/Av�A�AK�A�A
�`A
ZA
$�A	��A	O�A	7LA	&�AĜAr�A�A\)A5?A`BAO�A7LAZAhsA;dA�`Az�A��A�A ��A   @�Ĝ@��^@��7@�@�j@�E�@��@���@�ȴ@�v�@��T@�@�O�@�V@���@�Ĝ@��`@���@�r�@��@�^5@�M�@�@�Q�@�V@�X@�ƨ@��#@���@�t�@�v�@�-@��#@�hs@���@���@�bN@��
@۝�@ۥ�@۶F@�S�@���@�|�@�E�@ڗ�@أ�@�X@ԋD@�j@�I�@��m@��@�J@с@�G�@�&�@��@�&�@�/@�/@�&�@��/@�I�@мj@���@�1@�  @���@�Z@�9X@�t�@���@Ͳ-@�I�@�E�@ɡ�@�-@�@�V@��@�^5@�=q@�I�@��;@�(�@ǥ�@���@���@�%@�hs@ř�@�X@ģ�@�  @öF@ÍP@�dZ@°!@+@�ff@�=q@�@���@��@�r�@��;@�\)@�+@�
=@��!@�^5@�$�@���@�hs@�7L@��@�I�@��@�  @���@��@���@���@�ff@��@���@�O�@�?}@��@��j@�1@��@�C�@��H@�~�@�{@��#@��^@���@��7@�O�@�Z@��@��
@��w@���@���@�~�@���@��@��u@��@�Q�@� �@�1@�ƨ@�C�@��@�
=@��@��y@��\@�=q@��#@��@�O�@��/@�z�@�  @�|�@��@��R@�ff@��@���@�(�@�b@�1@�  @��@��@�l�@���@��@���@�G�@�V@�Ĝ@��u@�Z@� �@���@��@�C�@�o@�@���@��@��R@��\@�n�@�E�@�{@��h@�7L@���@��D@��@���@���@��+@�=q@��@��@�@�`B@���@���@���@�r�@� �@��m@���@�\)@���@���@��+@�=q@��@���@�p�@�O�@�?}@��@���@��`@��@�bN@�b@�\)@���@�ȴ@���@���@�ff@�5?@���@��7@�X@�%@��j@�r�@� �@��
@�\)@�33@�"�@�
=@���@�M�@�{@�@��T@��@�X@��@��@��j@�z�@�r�@�bN@�I�@�(�@� �@���@���@�S�@�o@��H@��\@�V@�J@��#@�x�@�G�@�V@��9@�z�@�Z@�9X@��
@�ƨ@���@�\)@�S�@�S�@�K�@�;d@�33@�+@�ȴ@�ff@�$�@��@�/@�r�@�  @��F@���@���@���@��P@�t�@�l�@�dZ@�\)@�\)@�S�@�C�@�C�@�;d@�o@�v�@��@���@���@�`B@�/@��@��u@�I�@��m@���@�l�@��@�ff@���@��7@�`B@�/@��@�bN@�Q�@�A�@�A�@�9X@�1'@� �@�b@�@��@�P@;d@~��@~��@~E�@}��@}O�@|��@|�@{��@{t�@{@z^5@z�@y�#@y7L@x��@x��@x�@xbN@x �@w;d@v��@v��@vv�@u��@u?}@t�/@t�D@t�D@tj@tI�@t9X@s��@s��@sdZ@so@r�\@q��@qhs@qG�@qG�@qG�@q�@q�@p��@pr�@o�@p  @o|�@n�y@nE�@m�@m/@m�@l��@l�@l��@k�
@kdZ@k33@k"�@k@j�H@j~�@j^5@j=q@j=q@j-@i�^@ix�@ix�@ix�@iG�@i7L@i�@hr�@h1'@hb@hb@g�@g�@f�@f�R@fE�@e�T@e�h@d��@d��@d1@ct�@co@b��@b^5@a��@`��@` �@_��@_|�@_+@^ȴ@^v�@^$�@]��@]O�@]/@\�@\��@\(�@[�
@[t�@["�@[o@[o@[o@[o@Z�@Z~�@Y�^@YG�@XĜ@X1'@W�w@W
=@V�+@VV@VE�@U�-@Up�@U`B@U?}@U�@T�/@TI�@T�@S�m@S��@SC�@S@R�!@Rn�@R-@Q��@QX@Q&�@PĜ@O�@O��@Ol�@O+@N�y@Nv�@N@M�h@M`B@L��@L�@Lj@L1@K�F@KC�@J�@J��@J��@J��@J��@J~�@J^5@J=q@JJ@I��@I�@H��@HbN@H �@G�;@G�@G�@Fȴ@F��@FV@FE�@F$�@F@E�T@E@Ep�@D��@D�D@D(�@C�
@Ct�@B��@BM�@B�@A�@A�^@A&�@@��@@��@@bN@?�P@?K�@?;d@?�@?
=@>�R@>ff@=��@=p�@=?}@<��@<�j@<�@<��@;ƨ@:�H@:�!@:n�@:M�@9��@9X@9%@8��@8�`@8�@8Q�@7�;@6�y@6V@5�@5p�@5/@4�@41@3�F@3�@3"�@2��@2^5@1x�@0��@0b@0  @0  @/�@/�@/�@/l�@/K�@/;d@/�@.��@.ff@.$�@.@-�@-@-�@-V@,�@,I�@+�F@+dZ@+S�@+33@*�H@*�@)�@)�^@)x�@)G�@(��@(�u@(r�@(Q�@(b@'�w@'��@'l�@'l�@'\)@'
=@&��@&ff@&5?@&{@%��@%�h@%O�@%O�@%/@%�@$�/@$�j@$�D@$�D@$z�@$j@$9X@$1@#��@#ƨ@#��@#t�@#C�@#@"�H@"��@"��@"n�@"=q@!��@!�^@!x�@!G�@!&�@ �`@ bN@ A�@ b@��@l�@+@�@��@v�@ff@$�@@�@��@�@`B@�@�@�/@�@�D@Z@��@33@o@�@�H@��@��@��@��@�\@~�@M�@-@�@��@��@hs@G�@%@�9@Q�@ �@b@b@b@  @�@�;@�@|�@
=@ȴ@��@�+@�+@V@�-@?}@�@V@�@�@j@�@��@��@�m@�m@�m@ƨ@t�@"�@@�@�H@�\@~�@M�@J@�@�^@G�@7L@�@��@��G�O�A�1A�
=A�JA�JA�oA�JA�oA�bA�VA�oA�JA�VA�bA�VA�oA�oA�VA�oA�VA�bA�{A�VA�oA�{A�VA�{A�oA�VA�{A�oA�bA�{A�VA�{A�oA�oA��A�{A�bA��A�bA�{A��A�oA��A�{A�oA��A�{A�oA��A�oA�{A��A�{A�{A��A�oA�{A��A�{A�{A��A�oA��A�{A��A��A�{A�{A��A��A�{A��A��A��A��A��A��A��A�{A��A��A�{A��A��A�{A��A��A�oA��A��A�{A��A��A��A��A��A��A��A�{A��A��A�oA��A�{A�bA�{A�oA�oA��A��A�{A��A�{A��A��A�{A��A��A�bA�{A��A�{A��A�{A�oA��A��A��A��A��A��A��A��A��A� �A��A��A�"�A��A��A�"�A��A��A� �A��A� �A� �A��A�"�A� �A��A�"�A� �A��A� �A�"�A��A�$�A�"�A��A�"�A�$�A��A�"�A�"�A��A�"�A�(�A�$�A�&�A�+A�(�A�$�A�+A�&�A�&�A�+A�(�A�+A�-A�+A�33A�=qA�9XA�=qA�A�A�=qA�?}A�C�A�?}A�I�A�E�A�G�A�M�A�M�A�VA�^5A�^5A�XA�^5A�ZA�\)A�dZA�`BA�ffA�jA�hsA�ffA�jA�jA�ffA�jA�l�A�ffA�l�A�l�A�hsA�jA�l�A�hsA�hsA�jA�hsA�hsA�jA�dZA�hsA�hsA�bNA�ffA�ffA�`BA�dZA�ffA�\)A�bNA�bNA�bNA�hsA�dZA�bNA�dZA�\)A�ZA�ZA�VA�O�A�Q�A�K�A�E�A�G�A�A�A�A�A�C�A�?}A�=qA�C�A�?}A�?}A�E�A�A�A�?}A�=qA�?}A�=qA�C�A�?}A�;dA�C�A�A�A�?}A�C�A�E�A�A�A�C�A�G�A�C�A�E�A�G�A�C�A�A�A�A�A�7LA�-A�/A�+A�+A�1'A�-A�+A�/A�-A�+A�/A�/A�+A�-A�1'A�/A�+A�-A�1'A�-A�-A�/A�+A�+A�1'A�+A�(�A�/A�+A�-A�1'A�-A�+A�/A�+A�&�A�"�A��A��A��mA��HA���A־wAְ!A֟�Aև+A�`BA�E�A�"�A��A��A��/A�ȴAհ!Aհ!Aթ�A�l�A�ffA�33A�+A��A�JA�
=A���A��A��mA��;A���A���A�ȴAԸRAԮAԬAԙ�AԑhAԉ7AԁA�v�A�jA�bNA�K�A�7LA�{A���A��;A���A���AӶFAӝ�Aӕ�A�~�A��A�&�A�{A���AҍPA�ffA�G�A�/A��A�
=A��A���AѼjAѰ!Aћ�AсA�dZA�A�A�(�A�{A���A��mA���AоwAУ�AЏ\A�x�A�hsA�dZA�ffA�`BA�S�A�S�A�M�A�9XA�5?A��A�
=A���A��TAϡ�A��A��A��#A��
A���AΧ�AΧ�AΡ�AΝ�AΛ�A΋DA�`BA��A͕�A�A̗�A�z�A�(�A���A�;dA�Aʇ+A�1'A�A��
Aɏ\A�p�A�`BA�^5A�VA�G�A�1'A�bA��A���Aȣ�Aȝ�Aș�Aȕ�AȋDAȅA�x�A�hsA�C�A�$�A�|�A�{A��
A�dZA�&�A�{A�bA�1A���A���A���AŸRAŰ!Ať�Aš�AŁA�bA�Aģ�AąA�`BA�`BA�XA�oA��#A��
Aã�Aß�AÇ+A�l�A�M�A�(�A�"�A��A���A��`A�A®A£�AhA�r�A�VA�A�A�A���A��7A�ffA�E�A�1'A��A���A��A���A�ĜA��FA���A���A���A��\A��7A��7A��DA��+A��A�|�A�|�A�z�A�v�A�t�A�r�A�p�A�t�A�ffA�dZA�`BA�dZA�`BA�VA�/A�-A� �A��A�VA��A�~�A�A�n�A�hsA�jA�bNA�I�A�;dA��`A��A�z�A�n�A�XA�/A� �A�A���A���A��A��;A��#A���A���A���A�r�A��A���A��A��`A���A��+A�l�A�^5A�?}A�$�A�VA���A��
A���A���A�|�A�dZA�E�A�oA�ĜA���A�ZA�/A�JA��;A�JA���A��wA��DA�ZA�+A�$�A��A��A�t�A�|�A�z�A�ffA�=qA�;dA�$�A��A��A��/A��/A���A�ƨA�t�A�`BA�  A��wA�p�A���A�oA�\)A�&�A���A���A�t�A�O�A�;dA�"�A�1A���A���A��A��TA��#A��A�ȴA���A��RA��A��7A�ffA�XA�Q�A��A���A���A��!A��A�n�A��A��A�ZA�&�A��A��hA�+A���A��A��HA���A�|�A�%A���A�~�A�`BA�/A��/A�bNA�/A�oA�  A���A��`A�ȴA���A��uA��7A��+A��7A��A�p�A�ffA�5?A�bA�  A��A��A��
A�A��-A�O�A��A��9A�hsA�-A���A��
A��9A�l�A���A���A���A��\A�|�A�z�A�t�A�p�A�p�A�ffA�C�A��A��mA���A��A��DA��A��A�v�A�dZA�XA�K�A�9XA�$�A�A��yA���A��DA�r�A�`BA�G�A�"�A�JA��HA��-A�|�A�S�A�(�A�JA��A���A��-A��A�O�A�&�A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                              ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BB�BA�BB�BB[BA�BB�BA�BB[BB[BB'BB�BB�BB�BC-BB�BB�BC�BCaBC-BC-BC-BCaBD3BHKBK�BN�BQNBS[BV�B`BkQBu%B�SB��B��B��B�B�FB��B�RB��B� B��B��B͟B��B�B�vB�BĜB�dB�BB�aB�BAB#�B.�B:^BM6Bn/B�$B�3B�[B�&B��B��B�&B�;B��B��B��B��B��B�iB�B��B�B��B�fB��B��B��B�B�B��B�<B��B��B�B��B��B��B�B��B��B��B��B�@B��B��B��B�SB�ABxBjKB^jBTaBA�B5tB&�B�BB�/B�B��B�qB�'B��B��BcBp;Bh>BT�B7LB)�B \B;B
�8B
�QB
ӏB
��B
�YB
�B
n�B
c�B
WsB
S�B
OvB
G�B
6�B
,�B
&�B
JB
�B	�DB	�iB	��B	ƨB	��B	��B	�B	y	B	p�B	[�B	U�B	T,B	S&B	QNB	L�B	MjB	K)B	<�B	9$B	6B	33B	2�B	0�B	.�B	)_B	'�B	"�B	�B	OB	 'B	�B	�B	�B	�B	�B	B	B	FB	oB	4B	(B	�B	"B	
�B	�B	�B	1B	B	�B		�B	
	B		7B	
	B		�B	
rB	
�B	
=B	
=B	�B	PB	VB	�B	_B	7B	~B	 \B	�B	CB	qB	7B	eB	�B	B	�B	�B	1B		B	!-B	.�B	9$B	LdB	N<B	NpB	NB	M�B	L0B	K�B	I�B	LdB	J�B	I�B	H�B	I�B	GzB	G�B	GzB	IB	L�B	M�B	MB	N�B	MjB	C�B	C-B	DgB	H�B	GzB	F�B	EB	C�B	E�B	FB	H�B	MB	S&B	T�B	^�B	b�B	d�B	i�B	l�B	k�B	m]B	k�B	d�B	`�B	a|B	d�B	gmB	iDB	iyB	gB	g�B	kB	n/B	m�B	m�B	m�B	o B	qvB	tB	uZB	v`B	{�B	�GB	~�B	��B	��B	� B	�4B	��B	�B	��B	��B	�_B	�_B	�DB	�JB	�B	��B	� B	��B	�{B	��B	�MB	��B	��B	�XB	��B	�0B	�hB	�*B	�*B	��B	��B	��B	�[B	��B	��B	�?B	�tB	��B	��B	��B	�B	ѷB	�B	�QB	�aB	ԕB	�KB	�B	�&B	�fB	�B	�WB	�]B	�)B	�/B	�B	�MB	��B	��B	�2B	��B	��B	��B	�VB
  B
oB
�B
{B
�B
�B
�B
B
�B
�B
�B
1B
1B
�B
_B
	B
B
"B
bB
B
@B
oB
oB
�B
@B
B
�B
B
�B
�B
�B
eB
�B
�B
kB
VB
�B
�B
�B
�B
�B
B
�B
OB
OB
"4B
$@B
$�B
$�B
%�B
&�B
&�B
'�B
(�B
)�B
*�B
+6B
+�B
,�B
,�B
.}B
/B
0�B
1�B
2�B
2�B
2�B
2�B
4nB
5tB
5�B
6B
6FB
6zB
7LB
6�B
9�B
9�B
8�B
9XB
:�B
:�B
:�B
:�B
;0B
;dB
<�B
<�B
=qB
=�B
=�B
=�B
=qB
=�B
=�B
>B
>B
?}B
?�B
?�B
@�B
@�B
A B
A�B
A�B
A�B
B'B
B'B
B'B
C�B
C�B
C�B
C�B
C�B
D�B
EB
E9B
E�B
FB
F�B
GB
GB
H�B
H�B
IB
IB
IB
I�B
K)B
K�B
LdB
NB
NB
N�B
MjB
NB
N<B
NpB
NpB
NpB
N�B
OBB
OBB
PB
PHB
PHB
Q�B
R�B
R�B
RTB
RTB
RTB
S[B
S�B
T,B
S�B
T,B
TaB
TaB
T�B
T�B
U�B
V9B
VB
V9B
VmB
W
B
W?B
YKB
YB
Y�B
ZQB
Y�B
YB
Y�B
[�B
[�B
[#B
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
^B
^�B
_�B
`B
`BB
_�B
_�B
`�B
`�B
a|B
a�B
cTB
cTB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d&B
c�B
c�B
dZB
d&B
d&B
dZB
e,B
f2B
ffB
e�B
f2B
f2B
ffB
f2B
gB
f�B
g�B
gmB
g8B
h�B
hsB
iyB
iB
iB
iB
kB
jB
jB
j�B
j�B
jB
j�B
jB
j�B
kB
j�B
kQB
k�B
lWB
l�B
l�B
m�B
m�B
m�B
n/B
m)B
m)B
m]B
m)B
m�B
m�B
o B
o B
o5B
o5B
o5B
o5B
p;B
o�B
oiB
oiB
oiB
o�B
o�B
pB
pB
pB
pB
pB
p;B
poB
qB
p�B
qAB
qvB
q�B
q�B
q�B
q�B
rGB
q�B
r|B
r�B
rGB
sMB
s�B
s�B
s�B
tB
s�B
s�B
tB
tTB
tTB
u%B
u%B
t�B
u%B
u%B
uZB
u�B
u�B
u�B
u�B
u�B
v�B
v�B
wfB
w�B
xB
w�B
x8B
x�B
x�B
x�B
x�B
x�B
y	B
yrB
zDB
{JB
{B
{B
|B
|�B
}�B
~(B
~(B
~]B
~�B
~�B
�B
�iB
��B
�B
�;B
�B
��B
�B
�uB
��B
�uB
��B
��B
�B
��B
�{B
�GB
�GB
�{B
�GB
�{B
��B
��B
��B
��B
��B
��B
�MB
�MB
��B
�B
��B
��B
�B
�SB
�SB
�SB
��B
��B
�%B
��B
�YB
�%B
��B
��B
��B
��B
��B
��B
�1B
�B
��B
�	B
��B
��B
�lB
�lB
��B
�	B
�=B
��B
��B
��B
��B
�~B
��B
�PB
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
�VB
�"B
�"B
�VB
�VB
��B
�(B
�\B
��B
��B
��B
��B
��B
��B
�.B
��B
� B
�hB
��B
�B
��B
��B
��B
�B
�B
��B
��B
��B
�FB
��B
��B
��B
�B
��B
�MB
�MB
�B
��B
�B
�SB
�SB
�SB
�B
�$B
��B
��B
�+B
�+B
�_B
��B
��B
��B
��B
�eB
�eB
��B
�	B
��B
�qB
��B
��B
�xB
��B
�B
�B
�~B
��B
��B
��B
��B
�'B
�'B
�'B
�'B
��B
��B
��B
��B
��B
��B
�bB
��B
��B
��B
��B
��B
�4B
�hB
��B
��B
�nB
�nB
�nB
�nB
��B
�tB
�tB
��B
��B
�B
�zB
��B
��B
��B
�B
�LB
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
�XB
�XB
�XB
��B
��B
��B
��B
�*B
�*B
�*B
�*B
��B
��B
��B
��B
��B
�0B
�eB
�eB
��B
�B
�B
�kB
�kB
��B
�B
�B
�B
�B
�qB
��B
��B
��B
�wB
�wB
��B
�IB
��B
��B
��B
�B
�OB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�-B
�-B
�aB
�aB
��B
��B
�3B
��B
��B
��B
�9B
�9B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�tB
��B
��B
�B
�zB
�B
�zB
�LB
�LB
�LB
��B
��B
�LB
�RB
�RB
�RB
��B
��B
�RB
��B
��B
��B
�$B
�XB
��B
��B
�*B
��B
�^B
�^B
��B
��B
�0B
�dB
�dB
��B
��B
�6BB�BCaBB�BB�BA BB�BAUBB'BB�B@�BC�BB[BA�BB�BA BA�BC-BAUBCaBB�BAUBB�BB[BAUBCaBA BA�BCaBAUBB�BC-BA�BC�BAUBB�BC-BA BB'BC-BA�BCaBB�BA�BC-BA�BB'BC�BA�BB�BC�BA�BCaBC-BA�BB�BCaBA�BC�BC-BA�BC�BC�BB[BD3BA�BC�BB�BA�BC�BC�BA�BB�BC�BB[BB�BC�BB[BC�BC-BC-BD�BB[BC-BD3BB'BC-BDgBB[BC�BDgBB[BB[BDgBB'BB�BC�BB�BB�BD�BB'BC�BB�BA�BDgBB�BB�BC�BB�BC-BCaBB�BB�BC�BB'BC�BCaBA�BEBB�BB[BD3BC�BB�BDgBB[BC�BD�BEBD�BE�BGBGBHKBFtBH�BI�BGzBI�BK^BIBL�BJ�BIRBM�BN�BN<BO�BM�BN<BOvBNpBN<BP}BN�BPHBR BP�BP�BR�BPHBP}BR�BQ�BQ�BS[BQ�BQ�BS[BR BS�BXBV�BU2BV9BXEBU�BVmBW�BUgBW�BW�BW?BYKBWsB\�Be,Bh>Bf2BhsBgmBg�BjBi�BkBkQBm]Bl"Br�Br|Bs�Bt�BrBrBt�BtTBy>B��B��B��B��B�MB�MB��B��B�SB��B��B�%B�1B��B��B�1B��B��B��B�	B��B�B�lB��B�~B��B�DB�VB�PB�PB��B�4B��B��B��B��B�CB�qB�OB��B�\B�4B�tB�B�B��B��B�0B��B�qB�B��B��B�-B��B�hB�tB�zB�zB�B�*B�XB�B�BB��B�B�B�}B�HB��B��B�B��B� B��B��BBÖB��B��B��BΥB�<B�dB��B�pB��B͟B�B�B�jBΥB�B�dB��B��B��B̘B�B�<B�BΥB�pB�dB��B�pB�0B�B�jB��B�6B��B��B̘B�B�pB͟B��B�NB��B�<B�BB�B�dB�,B�TB�B��BʌB��B��B�zB��B��B��B� B��B�B�<B�BB��B��B�qB��B��B�XB�0B��B�XB�wB�OB�}B�'B�aB�-B��BÖB��B�'B�-B�gBǮB�3B�mB��B�UB�BǮBŢBǮB�?B�}B��B�B�B
	BB�B�B+BB"�B!�B"�B'�B(�B,=B*�B+�B/OB1'B.�B/�B/�B33B0!B5?B5�B8�B7�B9�B>�B=�B?}BB'B?HBE9BEBGBD�BPBd�BW�BT�BT�BZ�Bg�Bm�Bn/BwfB|PB��B� B�B�=B��B�AB��B��B�hB��B��B��B��B��B�tB�vB��B�B�B�HB�,BԕB՛B�QB�)B�
BѷB҉B�&B��B��B�B�?B�]B��B��B��B�8B�B�;B�B�HB�B� B�HB��B��B�B��B�5B�B�vB�ZB�2B��B�|BٴB��B�ZB�jB�B��BуB�jB�B�gB�RB��B�#B��B�-B�wB�<B��B��B�B�aBƨB�^B��B�BΥB��B�,B�KB�HB�`B�,B��B�B�WB�KB��B��B�B�5B�B�B�B�B�B�;B�B�B�vB�B�B�B��B�B��B�iB�B�+B�2B�/B��B�B�TB�B�B�MB��B�AB�B�;B��B��B��B�JB�TB�B�B�ZB�JB�+B�|B�MB��B�B��B�JB�GB�"B�B��B��B��B��B�|B�B�B�B�B��B�B�|B��B��B�B�/B��B��B��B�WB��B�iB��B�)BoB�B�ZBٴB�B� B��B�?B�`B� B�wB�0B�,BɺB��B�XBɆB��BҽB�qB�dB�'B��B��B��B�<B�*B�0B�B��B��B�zB��B�B��B�B�FB�zB�LB��B��B��B��B�B�LB��B�RB��B��B��B��B��B�HB��B��B�FB��B��B�B�pBŢB�dB��B�B��B�*B��B��B��B�mBŢB�B�$B�9B��B�B�B��B�B�B�B��B�OB�}B�0B��B�XB�LB��B��B��B��B��B�FB��B��B��B��B�hB�6B�tB��B�'B�=B��B�B�FB��B��B�uB�AB�	B��B��B�GB��B�GB��B��B��B�DB��B�rB��B�{B�B��B��B��B�B�{B��B��B�;B�	B~�BcBy	B|�By�Bw�Bt�Bu�Bs�Bo5Bl�BiDBk�BgmBf�Bg�Bg8BaB^G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202104292026552021042920265520210429202655202104292026552021042920265520210429202655SI  SI  ARFMARFM                                                                                                                                                2020122920312020201229203120IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022416401220210224164012QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022416401220210224164012QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021042910194020210429101940IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021042920270220210429202702IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2021042920270220210429202702IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021042920270220210429202702IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                