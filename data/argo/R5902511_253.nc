CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:49:55Z creation      
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
resolution        =���   axis      Z          ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       b�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Ϙ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o      �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 4�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o      <p   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � [�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o      cP   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �4   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230721224955  20230721224955  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @��ٱ�0@��ٱ�011  @��  �@��  �@2L��AJM@2L��AJM�dě��o�dě��o11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  FF  ?�  ?��H@=p�@�  @�G�@�G�@�  A   A  A ��A,(�A>�RA_\)A\)A�  A�  A��A��A�  A�Q�A�A�\)B  B  B�
B (�B((�B0  B8  B@  BH(�BP(�BW�
B_�
Bh  Bo�
Bw�
B�(�B�{B�{B��
B�  B�(�B�{B�{B�(�B�(�B�{B�{B�{B�  B�  B�{B�  B��B��
B�  B�  B��B�  B�{B�{B��B�  B�(�B�  B��
B�  B�{B��C�C  C
=C
=C
{C{C��C��C  C
=C
=C
=C
=C  C��C��C"  C$  C&  C({C*  C+��C-��C0  C2  C4  C5�C7��C9��C;�C=��C?��CA�CD  CF{CH
=CI��CL  CM��CP
=CR  CT{CV  CX
=CZ  C\  C^
=C`{Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr
=Ct{Cv
=Cw�Cz  C{��C~
=C��C�  C���C�  C�C�  C�
=C�C���C�C�C�
=C�C�  C�  C�  C�  C�C�
=C�C�C�  C���C���C�  C�C�  C�  C�  C���C�  C�C�C�C�C�  C�C�
=C�  C�  C�  C�  C�C���C���C���C���C�  C�
=C�C�C�
=C�  C�C�  C�  C�  C���C�  C�C�
=C�  C���C�C�
=C�C�  C�  C���C���C�C�  C�C�C���C�  C���C���C�  C���C���C���C���C���C�  C�  C�  C�  C���C���C�C�C�C�C�C�C�C���C���C���C���C���C���C�  C�  C���C�C�
=C�
=C�
=C�C�C�  C���C�C�
=C�C���C���C�  C�  C�  C�  C�C�
=C�C�  C�D   D ��D�D� D�D� D�qD}qD  D� D�qD� D�D}qD�RDz�D�RDz�D	  D	� D	�qD
}qD  D� D  D��D  D� D  D� D�qDxRD��D}qD��Dz�D�qD��D�qDz�D��D}qD�qD� D�D��D  D� D�D�DD�DD�D�D}qD�qD� D  D��D�Dz�D��Dz�D�qD � D!�D!��D"�D"� D#  D#� D$  D$�D%�D%z�D%�qD&��D'  D'}qD(�D(��D)  D)� D)�qD*}qD*�qD+�D,  D,� D-  D-� D.�D.��D.�qD/z�D0  D0� D0�qD1��D2  D2� D3D3}qD3�qD4}qD5  D5��D6  D6� D7�D7�D8D8��D8�qD9z�D9�qD:��D;�D;�D<  D<� D=D=� D>  D>}qD?  D?��D?�qD@}qDA  DA� DB  DB� DC  DC� DD  DD}qDD�qDE��DF�DF}qDF�qDG}qDG�qDH��DI  DI}qDJ  DJ� DK�DK}qDK�qDL}qDL�RDM� DNDN�DODO� DP�DP�DQ  DQ}qDR  DR� DS  DS��DT  DT}qDU  DU�DV�DV� DV�qDWz�DW�qDXz�DX��DY� DY�qDZ� D[�D[��D\D\��D]�D]�D^  D^}qD_  D_��D_�qD`z�Da  Da� Db  Db� Db��Dc}qDd  Dd��De  De� Df�Df��Dg  Dg� Dh  Dh� Di�Di��Di�qDj}qDk  Dk��Dl  Dl��Dm  Dmz�Dn  Dn� Do  Do� Dp�Dp� Dq  Dq��Dr  Dr� DsDs��Ds�qDt� Du�Du}qDu�qDv� Dw�Dw� Dw�qDx}qDx�qDy� Dz  Dz}qD{  D{� D|  D|� D}  D}��D}�qD~z�D~�qD��D�  D�>�D�~�D��qD���D�@ D��HD���D��qD�@ D��HD�� D���D�@ D�� D�D�  D�=qD�~�D��HD�HD�>�D�� D��HD�  D�>�D�� D�� D�HD�AHD�}qD��qD�  D�AHD�� D��HD�HD�@ D�~�D�� D�  D�@ D�� D��HD�  D�@ D�� D���D�  D�@ D�}qD��qD��qD�@ D�� D�� D�  D�@ D�� D���D�  D�AHD��HD��HD�  D�>�D�� D�D��D�@ D�� D�� D�  D�@ D�~�D��qD���D�>�D�~�D���D�  D�>�D�~�D�� D�  D�@ D�~�D��qD���D�>�D��HD��HD�  D�B�D��HD�� D�HD�AHD��HD��HD�  D�AHD��HD�� D�  D�AHD��HD�� D�  D�@ D�� D�� D���D�@ D�� D��HD��D�AHD�� D���D�  D�AHD�� D�� D�  D�@ D��HD��HD���D�=qD�� D�� D��qD�@ D��HD��HD��D�AHD��HD�� D�  D�>�D�}qD���D�  D�@ D�� D��HD��D�@ D�~�D�� D���D�>�D�~�D�� D�HD�@ D�~�D���D���D�@ D���D�D�HD�AHD�� D�� D�  D�@ D��HD��HD�  D�>�D�� D�D�  D�>�D�~�D�� D�  D�>�D�� D���D���D�>�D�~�D���D���D�>�D�� D�� D�  D�AHD��HD�� D�HD�>�D�� D���D��qD�=qD�~�D��HD�HD�@ D�~�D�� D���D�@ D��HD��HD�  D�@ D�~�D���D�HD�@ D�~�D�� D�  D�>�D�� D��HD��D�@ D�� D��HD�HD�>�D�~�D���D��qD�@ D��HD���D���D�>�D�� D�� D���D�>�D�~�D���D�  D�B�D��HD��qD���D�@ D�� D�� D�  D�@ D�� D�� D���D�=qD�� D��HD�HD�>�D�~�D���D�  D�B�D�D�D�  D�@ DÀ Dþ�D�  D�AHDāHD��HD�  D�@ D�~�D�� D�HD�AHDƁHD��HD�HD�AHDǁHD�� D�HD�@ DȀ D�� D�  D�@ D�~�DɽqD���D�@ D�~�Dʾ�D�HD�B�Dˀ D�� D���D�>�D̀ D̾�D�  D�@ D́HD�� D���D�AHD΁HD�� D���D�@ Dπ D��HD��D�AHDЀ D�� D���D�@ DсHD��HD�  D�>�DҁHD��HD�  D�>�D�}qDӽqD�  D�AHDԀ D�� D�  D�=qD�~�Dվ�D��qD�>�Dր D��HD�HD�AHDׁHD׾�D���D�@ D؀ Dؾ�D�  D�AHD�~�Dپ�D���D�=qD�~�D�� D�  D�AHDہHD��HD�  D�>�D�~�D�� D�  D�@ D݁HD�D�HD�@ D�~�D޾�D�HD�@ D�~�D�� D�  D�>�D�~�D�� D�  D�>�D� D��HD�HD�AHD�~�D⾸D���D�>�D� D��HD�  D�=qD�}qD�� D��D�B�D� D�� D���D�>�D�HD��HD���D�@ D� D�� D�  D�@ D� D�� D�HD�@ D� D��HD�  D�>�D� D�� D�  D�@ D�~�D�qD�  D�@ D� D쾸D���D�>�D�~�D���D���D�AHD� D�qD���D�@ D�~�D�� D�HD�>�D�� D��HD���D�>�D� D�� D�HD�@ D� D��HD�  D�>�D�~�D�� D���D�@ D�HD�� D�HD�C�D��HD���D�  D�@ D�� D�� D�HD�=q>�G�?.{?k�?���?��?��@�@#�
@5@E�@\(�@s33@�  @��@�Q�@�  @���@�@�  @�ff@�z�@�p�@�ff@�z�@��RA33AQ�A�RA�\A
=A��A!�A%A,(�A0��A5�A;�A@  ADz�AJ�HAP  AS33AX��A^�RAa�Ag
=Amp�Ap��Au�Az�HA\)A���A��A�\)A���A���A�\)A���A�(�A��A��A�z�A�  A�=qA�z�A��A��\A�z�A�  A��\A���A�Q�A��HA��A�Q�AÅA�p�A�Q�A˅A�p�AУ�A��
A�{Aأ�A�(�A�ffA��A�(�A�ffA��A�A�RA��A�A�
=A���A�(�A�\)B ��B{B�B�B�B\)B	�B
�RB�B�B�RB  BG�B
=B��B��B33B��B�B33B�BffB�B!�B"�RB#�
B%p�B'
=B(Q�B)p�B*�HB,z�B-�B/
=B0��B2=qB3�B5�B6�HB8  B9p�B;
=B<��B=��B?\)B@��BA�BC�BE�BF=qBH  BIG�BJffBK�
BM��BN�\BP  BQ��BR�HBT  BUBW\)BXz�BYB[�B\��B]�B_�Ba�Bb{Bc�Be�BfffBg�Bi�Bj�RBk�Bl��BnffBo�Bp��Br=qBs�Bt��BuBw�Bx��ByB{
=B|��B~=qB\)B�=qB��B��B�Q�B���B��B�z�B��B�B��\B�G�B��
B�z�B�\)B��B�z�B�33B�  B�z�B��B�  B���B�33B��B��RB�\)B��B��RB��B�=qB���B�p�B�=qB���B��B�(�B�
=B��B�=qB��HB��B�Q�B��HB��B�ffB�
=B��B�Q�B�
=B�B�=qB�
=B��B�z�B�
=B��B��RB�G�B��B���B��B�(�B��RB�p�B�Q�B�
=B���B�=qB�
=B��
B�z�B��B��B���B�33B��
B��RB��B�{B��RB�\)B�Q�B���B���B�=qB��B�B�Q�B��B��B���B��B�B���B�33B�B�ffB��BîB�{B���BŅB�  B�z�B�
=BǮB�=qBȸRB�
=B�B�=qBʏ\B�
=B˙�B��B�=qB̸RB�G�BͅB�B�Q�BθRB�
=B�G�B�B�=qB�z�B���B�G�B�B�  B�=qBҸRB�33BӅB�B�(�Bԣ�B�
=B�33BՅB�{B�ffB֣�B���B�p�Bי�B��B�ffB���B�
=B�G�B��
B�{B�=qBڸRB��B�\)Bۙ�B�{B�ffB܏\B��HB�\)B݅B�B�(�Bޣ�B��HB�
=B߅B�  B�=qB�z�B��HB�\)B�B��
B�ffB��B��HB�\)B��
B�  B�ffB���B�33B�B�  B�z�B��B��B癚B�  B�Q�B�\B��B�B�B�(�B��B��B�\)B��
B�Q�B�z�B�
=B�B�B�(�B�RB�
=B�\)B��B�Q�B�RB��B�B�{B�Q�B�
=B�G�B�B�=qB��RB���B�p�B�  B�z�B��RB��B�B�(�B�z�B��HB��B�B�(�B���B�33B��B�{B���B��HB�\)B�  B�Q�B���B�33B��
C �C G�C z�C �RC  C=qCffC��C��C33C\)C��C�C{CG�C��C�
C{CG�Cz�C�RC  C=qCz�C��C�HC33CffC��C��C
=CQ�C�\C�RC��CG�Cz�C��C�
C	�C	ffC	��C	��C	��C
=qC
�\C
�RC
�HC{C\)C��C�HC
=C33Cz�C�RC�C{C\)C��C��C  C33Cz�C�RC  C=qCp�C��C��C{CQ�C�\C��C  C(�CffC�C�C(�CQ�Cz�C�RC��C=qC\)C��C�
C{C=qCffC��C�
C�CQ�Cp�C��C�
C
=CQ�C�CC�HC
=CG�Cz�CC  C(�CQ�Cz�CC��C=qCp�C��CC�C�CQ�C��C��C��C�CG�Cz�CC  C=qCffC�\C�RC�C(�CffC��C�
C  C(�CffC�C�HC{C=qCp�C��C�
C {C Q�C �C �C �HC!
=C!=qC!z�C!�RC!�C"{C"=qC"�\C"C"��C#(�C#Q�C#�C#�RC$  C$=qC$p�C$�C$�HC%
=C%33C%p�C%�RC%��C&(�C&Q�C&z�C&�RC&��C'33C'z�C'�RC'�C((�C(\)C(��C(��C)  C)(�C)p�C)�C)��C*=qC*p�C*�C*�C+(�C+\)C+�\C+C,{C,\)C,��C,�
C-
=C-=qC-p�C-�C-�C.(�C.p�C.�RC.��C/33C/p�C/��C/�
C0
=C0Q�C0��C0�HC1�C1\)C1�\C1C1��C233C2p�C2�C2�C3=qC3z�C3�RC3�C4(�C4p�C4�C4�C5(�C5\)C5�\C5C5��C633C6z�C6C7
=C7G�C7�C7C7��C8(�C8ffC8�C8�HC9�C9Q�C9�\C9��C:
=C:Q�C:�\C:��C;
=C;G�C;z�C;�C;�HC<{C<ffC<��C<�HC=(�C=Q�C=�C=�RC=��C>=qC>z�C>�RC>��C?33C?\)C?�\C?��C@
=C@Q�C@�\C@��CA
=CA=qCAp�CA��CA�
CB{CBG�CB�\CBCC
=CCG�CC�CC�RCC�CD{CDG�CD�CD�RCD��CE33CEz�CECF  CF=qCFp�CF��CF�HCG{CGG�CG�CG�RCH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                          ?�  ?��H@=p�@�  @�G�@�G�@�  A   A  A ��A,(�A>�RA_\)A\)A�  A�  A��A��A�  A�Q�A�A�\)B  B  B�
B (�B((�B0  B8  B@  BH(�BP(�BW�
B_�
Bh  Bo�
Bw�
B�(�B�{B�{B��
B�  B�(�B�{B�{B�(�B�(�B�{B�{B�{B�  B�  B�{B�  B��B��
B�  B�  B��B�  B�{B�{B��B�  B�(�B�  B��
B�  B�{B��C�C  C
=C
=C
{C{C��C��C  C
=C
=C
=C
=C  C��C��C"  C$  C&  C({C*  C+��C-��C0  C2  C4  C5�C7��C9��C;�C=��C?��CA�CD  CF{CH
=CI��CL  CM��CP
=CR  CT{CV  CX
=CZ  C\  C^
=C`{Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr
=Ct{Cv
=Cw�Cz  C{��C~
=C��C�  C���C�  C�C�  C�
=C�C���C�C�C�
=C�C�  C�  C�  C�  C�C�
=C�C�C�  C���C���C�  C�C�  C�  C�  C���C�  C�C�C�C�C�  C�C�
=C�  C�  C�  C�  C�C���C���C���C���C�  C�
=C�C�C�
=C�  C�C�  C�  C�  C���C�  C�C�
=C�  C���C�C�
=C�C�  C�  C���C���C�C�  C�C�C���C�  C���C���C�  C���C���C���C���C���C�  C�  C�  C�  C���C���C�C�C�C�C�C�C�C���C���C���C���C���C���C�  C�  C���C�C�
=C�
=C�
=C�C�C�  C���C�C�
=C�C���C���C�  C�  C�  C�  C�C�
=C�C�  C�D   D ��D�D� D�D� D�qD}qD  D� D�qD� D�D}qD�RDz�D�RDz�D	  D	� D	�qD
}qD  D� D  D��D  D� D  D� D�qDxRD��D}qD��Dz�D�qD��D�qDz�D��D}qD�qD� D�D��D  D� D�D�DD�DD�D�D}qD�qD� D  D��D�Dz�D��Dz�D�qD � D!�D!��D"�D"� D#  D#� D$  D$�D%�D%z�D%�qD&��D'  D'}qD(�D(��D)  D)� D)�qD*}qD*�qD+�D,  D,� D-  D-� D.�D.��D.�qD/z�D0  D0� D0�qD1��D2  D2� D3D3}qD3�qD4}qD5  D5��D6  D6� D7�D7�D8D8��D8�qD9z�D9�qD:��D;�D;�D<  D<� D=D=� D>  D>}qD?  D?��D?�qD@}qDA  DA� DB  DB� DC  DC� DD  DD}qDD�qDE��DF�DF}qDF�qDG}qDG�qDH��DI  DI}qDJ  DJ� DK�DK}qDK�qDL}qDL�RDM� DNDN�DODO� DP�DP�DQ  DQ}qDR  DR� DS  DS��DT  DT}qDU  DU�DV�DV� DV�qDWz�DW�qDXz�DX��DY� DY�qDZ� D[�D[��D\D\��D]�D]�D^  D^}qD_  D_��D_�qD`z�Da  Da� Db  Db� Db��Dc}qDd  Dd��De  De� Df�Df��Dg  Dg� Dh  Dh� Di�Di��Di�qDj}qDk  Dk��Dl  Dl��Dm  Dmz�Dn  Dn� Do  Do� Dp�Dp� Dq  Dq��Dr  Dr� DsDs��Ds�qDt� Du�Du}qDu�qDv� Dw�Dw� Dw�qDx}qDx�qDy� Dz  Dz}qD{  D{� D|  D|� D}  D}��D}�qD~z�D~�qD��D�  D�>�D�~�D��qD���D�@ D��HD���D��qD�@ D��HD�� D���D�@ D�� D�D�  D�=qD�~�D��HD�HD�>�D�� D��HD�  D�>�D�� D�� D�HD�AHD�}qD��qD�  D�AHD�� D��HD�HD�@ D�~�D�� D�  D�@ D�� D��HD�  D�@ D�� D���D�  D�@ D�}qD��qD��qD�@ D�� D�� D�  D�@ D�� D���D�  D�AHD��HD��HD�  D�>�D�� D�D��D�@ D�� D�� D�  D�@ D�~�D��qD���D�>�D�~�D���D�  D�>�D�~�D�� D�  D�@ D�~�D��qD���D�>�D��HD��HD�  D�B�D��HD�� D�HD�AHD��HD��HD�  D�AHD��HD�� D�  D�AHD��HD�� D�  D�@ D�� D�� D���D�@ D�� D��HD��D�AHD�� D���D�  D�AHD�� D�� D�  D�@ D��HD��HD���D�=qD�� D�� D��qD�@ D��HD��HD��D�AHD��HD�� D�  D�>�D�}qD���D�  D�@ D�� D��HD��D�@ D�~�D�� D���D�>�D�~�D�� D�HD�@ D�~�D���D���D�@ D���D�D�HD�AHD�� D�� D�  D�@ D��HD��HD�  D�>�D�� D�D�  D�>�D�~�D�� D�  D�>�D�� D���D���D�>�D�~�D���D���D�>�D�� D�� D�  D�AHD��HD�� D�HD�>�D�� D���D��qD�=qD�~�D��HD�HD�@ D�~�D�� D���D�@ D��HD��HD�  D�@ D�~�D���D�HD�@ D�~�D�� D�  D�>�D�� D��HD��D�@ D�� D��HD�HD�>�D�~�D���D��qD�@ D��HD���D���D�>�D�� D�� D���D�>�D�~�D���D�  D�B�D��HD��qD���D�@ D�� D�� D�  D�@ D�� D�� D���D�=qD�� D��HD�HD�>�D�~�D���D�  D�B�D�D�D�  D�@ DÀ Dþ�D�  D�AHDāHD��HD�  D�@ D�~�D�� D�HD�AHDƁHD��HD�HD�AHDǁHD�� D�HD�@ DȀ D�� D�  D�@ D�~�DɽqD���D�@ D�~�Dʾ�D�HD�B�Dˀ D�� D���D�>�D̀ D̾�D�  D�@ D́HD�� D���D�AHD΁HD�� D���D�@ Dπ D��HD��D�AHDЀ D�� D���D�@ DсHD��HD�  D�>�DҁHD��HD�  D�>�D�}qDӽqD�  D�AHDԀ D�� D�  D�=qD�~�Dվ�D��qD�>�Dր D��HD�HD�AHDׁHD׾�D���D�@ D؀ Dؾ�D�  D�AHD�~�Dپ�D���D�=qD�~�D�� D�  D�AHDہHD��HD�  D�>�D�~�D�� D�  D�@ D݁HD�D�HD�@ D�~�D޾�D�HD�@ D�~�D�� D�  D�>�D�~�D�� D�  D�>�D� D��HD�HD�AHD�~�D⾸D���D�>�D� D��HD�  D�=qD�}qD�� D��D�B�D� D�� D���D�>�D�HD��HD���D�@ D� D�� D�  D�@ D� D�� D�HD�@ D� D��HD�  D�>�D� D�� D�  D�@ D�~�D�qD�  D�@ D� D쾸D���D�>�D�~�D���D���D�AHD� D�qD���D�@ D�~�D�� D�HD�>�D�� D��HD���D�>�D� D�� D�HD�@ D� D��HD�  D�>�D�~�D�� D���D�@ D�HD�� D�HD�C�D��HD���D�  D�@ D�� D�� D�HD�=q>�G�?.{?k�?���?��?��@�@#�
@5@E�@\(�@s33@�  @��@�Q�@�  @���@�@�  @�ff@�z�@�p�@�ff@�z�@��RA33AQ�A�RA�\A
=A��A!�A%A,(�A0��A5�A;�A@  ADz�AJ�HAP  AS33AX��A^�RAa�Ag
=Amp�Ap��Au�Az�HA\)A���A��A�\)A���A���A�\)A���A�(�A��A��A�z�A�  A�=qA�z�A��A��\A�z�A�  A��\A���A�Q�A��HA��A�Q�AÅA�p�A�Q�A˅A�p�AУ�A��
A�{Aأ�A�(�A�ffA��A�(�A�ffA��A�A�RA��A�A�
=A���A�(�A�\)B ��B{B�B�B�B\)B	�B
�RB�B�B�RB  BG�B
=B��B��B33B��B�B33B�BffB�B!�B"�RB#�
B%p�B'
=B(Q�B)p�B*�HB,z�B-�B/
=B0��B2=qB3�B5�B6�HB8  B9p�B;
=B<��B=��B?\)B@��BA�BC�BE�BF=qBH  BIG�BJffBK�
BM��BN�\BP  BQ��BR�HBT  BUBW\)BXz�BYB[�B\��B]�B_�Ba�Bb{Bc�Be�BfffBg�Bi�Bj�RBk�Bl��BnffBo�Bp��Br=qBs�Bt��BuBw�Bx��ByB{
=B|��B~=qB\)B�=qB��B��B�Q�B���B��B�z�B��B�B��\B�G�B��
B�z�B�\)B��B�z�B�33B�  B�z�B��B�  B���B�33B��B��RB�\)B��B��RB��B�=qB���B�p�B�=qB���B��B�(�B�
=B��B�=qB��HB��B�Q�B��HB��B�ffB�
=B��B�Q�B�
=B�B�=qB�
=B��B�z�B�
=B��B��RB�G�B��B���B��B�(�B��RB�p�B�Q�B�
=B���B�=qB�
=B��
B�z�B��B��B���B�33B��
B��RB��B�{B��RB�\)B�Q�B���B���B�=qB��B�B�Q�B��B��B���B��B�B���B�33B�B�ffB��BîB�{B���BŅB�  B�z�B�
=BǮB�=qBȸRB�
=B�B�=qBʏ\B�
=B˙�B��B�=qB̸RB�G�BͅB�B�Q�BθRB�
=B�G�B�B�=qB�z�B���B�G�B�B�  B�=qBҸRB�33BӅB�B�(�Bԣ�B�
=B�33BՅB�{B�ffB֣�B���B�p�Bי�B��B�ffB���B�
=B�G�B��
B�{B�=qBڸRB��B�\)Bۙ�B�{B�ffB܏\B��HB�\)B݅B�B�(�Bޣ�B��HB�
=B߅B�  B�=qB�z�B��HB�\)B�B��
B�ffB��B��HB�\)B��
B�  B�ffB���B�33B�B�  B�z�B��B��B癚B�  B�Q�B�\B��B�B�B�(�B��B��B�\)B��
B�Q�B�z�B�
=B�B�B�(�B�RB�
=B�\)B��B�Q�B�RB��B�B�{B�Q�B�
=B�G�B�B�=qB��RB���B�p�B�  B�z�B��RB��B�B�(�B�z�B��HB��B�B�(�B���B�33B��B�{B���B��HB�\)B�  B�Q�B���B�33B��
C �C G�C z�C �RC  C=qCffC��C��C33C\)C��C�C{CG�C��C�
C{CG�Cz�C�RC  C=qCz�C��C�HC33CffC��C��C
=CQ�C�\C�RC��CG�Cz�C��C�
C	�C	ffC	��C	��C	��C
=qC
�\C
�RC
�HC{C\)C��C�HC
=C33Cz�C�RC�C{C\)C��C��C  C33Cz�C�RC  C=qCp�C��C��C{CQ�C�\C��C  C(�CffC�C�C(�CQ�Cz�C�RC��C=qC\)C��C�
C{C=qCffC��C�
C�CQ�Cp�C��C�
C
=CQ�C�CC�HC
=CG�Cz�CC  C(�CQ�Cz�CC��C=qCp�C��CC�C�CQ�C��C��C��C�CG�Cz�CC  C=qCffC�\C�RC�C(�CffC��C�
C  C(�CffC�C�HC{C=qCp�C��C�
C {C Q�C �C �C �HC!
=C!=qC!z�C!�RC!�C"{C"=qC"�\C"C"��C#(�C#Q�C#�C#�RC$  C$=qC$p�C$�C$�HC%
=C%33C%p�C%�RC%��C&(�C&Q�C&z�C&�RC&��C'33C'z�C'�RC'�C((�C(\)C(��C(��C)  C)(�C)p�C)�C)��C*=qC*p�C*�C*�C+(�C+\)C+�\C+C,{C,\)C,��C,�
C-
=C-=qC-p�C-�C-�C.(�C.p�C.�RC.��C/33C/p�C/��C/�
C0
=C0Q�C0��C0�HC1�C1\)C1�\C1C1��C233C2p�C2�C2�C3=qC3z�C3�RC3�C4(�C4p�C4�C4�C5(�C5\)C5�\C5C5��C633C6z�C6C7
=C7G�C7�C7C7��C8(�C8ffC8�C8�HC9�C9Q�C9�\C9��C:
=C:Q�C:�\C:��C;
=C;G�C;z�C;�C;�HC<{C<ffC<��C<�HC=(�C=Q�C=�C=�RC=��C>=qC>z�C>�RC>��C?33C?\)C?�\C?��C@
=C@Q�C@�\C@��CA
=CA=qCAp�CA��CA�
CB{CBG�CB�\CBCC
=CCG�CC�CC�RCC�CD{CDG�CD�CD�RCD��CE33CEz�CECF  CF=qCFp�CF��CF�HCG{CGG�CG�CG�RCH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�C�A�;dA�?}A�=qA�;dA�9XA�5?A�/A�+A�&�A�$�A��A��A��A�ĜA�z�A�K�A�5?A�bA�\)A�oA��
A���A���A��
A��
A��
A��A���A���A���A��
A��/A��;A��TA��HA��#Aˡ�A˃A�dZA�ZA�G�A��TA�G�A��Aɩ�A�7LA�VA�ĜA�%AǅA���A�S�AŇ+A��HA�33A�&�A���A�1A��`A���A�"�A��
A�$�A�VA�jA�bNA���A�ƨA� �A��A��^A�VA���A�M�A��DA��`A���A��9A���A���A�bNA�C�A��A��HA��-A�&�A��A�hsA�ƨA�M�A�bA��A���A��yA�E�A��DA�`BA��A��A���A��RA�{A�l�A��A��FA�$�A`BA}�Ay�wAv��Asx�Ar�jAq��Ap��Al  Ah�Af1AbȴA^bNAZ$�AXJAU�TAR�\AP�`ANJAI�;AIS�AH�yAE�mABVABM�AB9XAB  A@�+A=x�A:��A8{A7?}A6�A5A4��A2jA/�;A.��A-A,A�A*I�A)�PA(�`A($�A'��A&�9A%��A%
=A$ĜA$-A#�hA#O�A!A jAhsA~�A-AO�AM�AO�A�DAv�A��A��A�jA7LAAffA��A��AVAbAXAJAO�A��Ap�A
��A
VA	�A	%A(�A`BAA�AȴA�A�`AM�A��A�A ��A �\A A�A @��@�+@���@��@���@�V@�S�@��@���@�o@��H@�-@���@�!@�~�@�^5@�j@��y@�hs@�@�+@��@�\@�n�@�^5@ꗍ@�R@�@�7L@�7L@��@�j@�9@�9@�@�r�@�I�@�9X@�9X@�1@�o@�@��;@�v�@���@�b@߮@�o@�M�@ݩ�@�V@���@�j@�
=@��@ش9@��;@�K�@��@���@�M�@���@�o@��H@��T@���@�O�@�;d@�-@���@ёh@Л�@�Z@�\)@Ͳ-@ͺ^@ͺ^@ͺ^@�r�@˶F@��@ʏ\@�=q@��#@�O�@ț�@��@�J@ʇ+@�n�@�5?@�@�7L@�x�@Ɂ@�O�@ȼj@�33@Ɵ�@�@�p�@Ĵ9@�I�@�  @�o@���@��@���@���@�/@��@�  @�A�@� �@�o@�V@���@��j@�(�@���@��@�o@��R@���@�ff@�V@�{@��@��@��@��@��P@��@��@��y@��y@���@���@�p�@��@��D@��m@�ƨ@��F@��P@��y@���@���@��h@�p�@�`B@��@���@�r�@�1'@���@�ƨ@�\)@�S�@�K�@�33@��@���@���@�v�@�=q@���@���@�p�@��j@��@��@��@�K�@�"�@��@�@��-@���@��7@�&�@��@���@���@��@�l�@�\)@�K�@��@�@��@�ȴ@���@�n�@�5?@��-@��@�X@�Ĝ@�j@�9X@��;@�l�@�33@�@��@���@�E�@�@�G�@���@���@�j@���@���@�C�@�
=@��!@�ff@��@�5?@�@��7@�O�@�G�@��@���@��@��u@��D@�r�@�1'@���@���@�S�@���@��+@�V@�$�@�J@��T@���@�?}@���@��/@��@�Q�@��
@��@�
=@��y@��R@��\@�M�@���@�x�@�O�@��@��@��@�Z@� �@��@���@�|�@�t�@�t�@�\)@�
=@���@�ff@�V@�=q@�{@���@��T@�hs@�%@��/@���@�Ĝ@��j@��D@�1@��
@��P@�C�@�"�@���@�ff@��@��@�O�@�&�@�V@���@��@��j@�Q�@���@��F@��P@�33@��y@���@�M�@��@���@�x�@�V@��u@��D@��@�z�@��@��@�j@�Z@�9X@��w@�;d@��\@�J@�@��@�O�@��@��/@���@��9@��@�j@�A�@��@�b@���@��@��y@�ȴ@��R@���@���@�ff@�E�@�@�G�@��@��@��@��@�V@�%@���@���@���@��@�r�@�9X@;d@~��@~�+@~$�@}�T@}@}��@}p�@}`B@}`B@}O�@}�@|��@|Z@{ƨ@{33@z�\@z^5@y��@y��@y��@y&�@x��@x�@w��@wl�@w\)@wK�@wK�@w�@v�+@v{@u��@u�-@u?}@t�/@t�D@s��@r��@r~�@q��@q�7@qG�@p��@p��@pQ�@o�@o��@n�@nE�@m@mp�@m`B@l�/@l�@l��@lz�@l(�@k��@j��@j�\@jn�@j�@i�@i��@i��@h�9@h1'@g��@g�w@g|�@g
=@f�R@fV@f@e�h@d�@dj@ct�@b�H@b~�@b�@a�@a��@a�7@aX@aG�@a7L@`�`@`�@`Q�@`b@_��@_�P@_+@^��@^E�@^{@]�T@]��@]�@]?}@]V@\�@\�@\�/@[�m@[dZ@Z�@Z~�@Z=q@Z-@Z�@Y��@X��@XĜ@XQ�@W�@W\)@V�@V{@Up�@UV@T��@T�D@T9X@S�F@St�@So@R�@R��@R��@R�@Q�^@QX@Q&�@Q%@P��@PĜ@P�u@Pr�@Pb@O��@Ol�@O+@N�@N��@NE�@N@M��@MO�@L�j@LZ@L�@K��@KS�@KS�@K33@K@J��@J~�@I�@Ihs@H��@HbN@HA�@Hb@G�P@G\)@G+@F��@F@Ep�@E/@D��@D�/@D�@Dj@C�m@Cƨ@C��@CC�@C@C@B�@B~�@BJ@A��@Ahs@A7L@A�@@ �@?��@?|�@?\)@?;d@?+@?�@>��@>@=�-@=�h@=/@<�@<z�@<1@;ƨ@;C�@;"�@:��@:�!@:M�@9��@9�#@9��@9��@9X@8��@7��@7�P@7l�@7\)@7\)@7
=@6��@6@5��@5/@4�/@41@3�F@3�@3dZ@3dZ@3dZ@3S�@2��@2�!@2�\@2=q@1�@17L@0��@0�`@0Ĝ@0��@0A�@0  @/�;@/��@/|�@.��@.ff@.{@-�@-�h@-�@,��@,��@,Z@,(�@,(�@,�@+t�@*��@*�@*J@)�@)�#@)��@)x�@)%@(�9@(�@( �@'�w@'�@'�P@'�P@';d@&�@&�R@&�+@&E�@&{@%�T@%�@%�h@$�/@$I�@$9X@$9X@$9X@$9X@$(�@$(�@$1@#�m@#ƨ@#��@#S�@#"�@"�H@"�\@"�\@"~�@"M�@"�@!�^@!X@ ��@ ��@ ��@  �@�;@��@|�@K�@�@��@V@5?@�@O�@�@��@��@�D@(�@1@�m@��@C�@@@�@��@^5@^5@=q@J@�@�#@��@��@x�@7L@%@��@��@�@bN@bN@b@�w@|�@;d@+@�@
=@��@�R@��@v�@V@5?@5?@5?@@��@�-@�-@��@/@�@z�@I�@��@�
@�F@t�@dZ@"�@�@�H@�H@��@�!@~�@=q@��@�@��@x�@X@&�@&�@%@Ĝ@��@bN@1'@ �@  @�@  @  @  @�@�@��@��@��@��@��@�P@+@
=@��@��@�y@�R@�+@ff@$�@@��@��@`B@O�@?}@V@V@�/@�A�O�A�S�A�=qA�;dA�?}A�;dA�9XA�?}A�A�A�?}A�;dA�?}A�;dA�=qA�?}A�;dA�7LA�9XA�9XA�5?A�;dA�7LA�/A�1'A�33A�-A�-A�/A�(�A�&�A�&�A�(�A�"�A�$�A��A��A� �A��A��A��A��A�oA��A��A��A��A��A��A�5?A�I�A�E�A�;dA���AυA�VA�VA��TAΛ�A�E�A�Aͧ�AͅA�|�A�hsA�VA�S�A�S�A�I�A�O�A�M�A�A�A�;dA�9XA�5?A�5?A�5?A�-A�/A�$�A�
=A�JA�JA���A��
ȂhA�ZA�9XA�1'A�+A� �A�"�A�$�A��A��A��A��;A��A��A���A���A���A��
A���A���A���A��
A���A���A��
A���A���A���A��
A���A���A��A���A���A��A��
A���A��A��A���A��
A��A��A���A��
A��#A��
A���A��#A��A��
A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A��
A��A��A��/A��/A��/A��/A��TA��;A��/A��HA��TA��/A��HA��`A��TA��;A��TA��`A��HA��TA��TA��HA��;A��HA��TA��HA��yA��A��A���A˰!A˰!AˮAˡ�A˙�A˟�A˙�A˕�AˑhAˏ\A˃A�r�A�l�A�hsA�ffA�bNA�bNA�ffA�`BA�\)A�\)A�^5A�VA�Q�A�O�A�M�A�G�A�E�A�G�A�C�A�=qA�9XA�"�A��`Aʧ�Aʏ\A�|�A�hsA�Q�A�;dA�1'A�&�A�oA�
=A���A��A��A��
A���AɸRAɣ�Aɧ�Aɥ�Aɗ�AɁA�`BA�1'A��A��A�{A�oA�VA�JA�VA�bA�JA���A���A��mAȇ+A�S�A�=qA�/A�"�A���A���A�ĜAǺ^AǃA�v�A�l�A�jA�`BA��A���A��A��#A���Aƥ�A�x�A�ffA�E�A�(�A���A���Ať�AŋDA�x�A�E�A�7LA�7LA� �A���A��TA��
Aĉ7A�ZA¬A�~�A�7LA�A��
A���A�hsA�S�A�{A��yA��uA�{A��-A��+A�XA�=qA�-A�"�A��A�JA��A�ȴA�x�A�;dA�/A�+A�(�A��A��A� �A��A�{A�1A���A���A���A��A��A��A��A��A��A��yA��TA��TA��`A��TA��#A��A��#A��
A���A���A���A�ƨA��wA���A��-A���A���A�z�A�^5A�?}A�"�A��A�l�A��-A�(�A��;A���A�\)A�K�A�G�A�A�A�%A�A���A���A���A���A��mA��
A�~�A�VA�=qA�9XA�&�A�"�A�$�A�&�A�"�A��A� �A��A��A�{A�1A���A��+A�M�A��A�A�  A���A��#A��;A�ȴA��^A���A��7A��A�\)A�-A��9A��TA�^5A�ȴA��\A���A��;A��/A���A���A���A���A���A�ƨA��wA��A���A��\A��A�r�A�\)A�K�A�{A�z�A���A� �A���A�?}A���A��hA�jA�VA�/A�JA���A���A���A�n�A�JA�ĜA���A�p�A�Q�A��A�oA�
=A��A�ĜA�A��jA��!A��9A���A���A��7A��A�hsA�ZA�K�A�O�A�M�A�K�A�?}A��A�1A��A��mA���A���A��-A���A�ffA�VA��uA���A�r�A�VA�1A�
=A��A��A��A��wA���A���A�~�A�hsA�r�A�ffA�l�A�-A�JA�A��mA��TA��HA��;A���A���A�hsA���A��
A���A���A��PA�~�A�^5A�C�A�+A�
=A���A��#A��A�jA�O�A�9XA��A���A��TA���A���A��uA��PA��A�r�A�1'A���A��A���A��jA��wA��wA��RA��hA�A�A���A��wA���A�VA�|�A�E�A�bA��HA���A��PA�7LA��`A�ƨA��RA��PA�1'A�VA��HA���A��^A���A��PA�p�A�S�A�/A�bA�A��A���A��^A��-A���A��+A�bNA���A���A���A��A�+A��/A�ĜA��wA��^A��9A��!A��-A��!A��A���A���A��DA��A�r�A�\)A�=qA��A��yA��/A���A��A��A�33A��yA���A��A�x�A��RA�A�A��A���A��wA���A��7A�z�A�v�A�t�A�I�A�%A�x�A�"�A��A��;A���A�ȴA�A���A�r�A�A�A�/A��A�%A�  A���A��jA��A�\)A�;dA�+A�"�A��A�bA�  A��A��TA��A�ƨA��!A���A�|�A�ffA�\)A�9XA�(�A� �A��A�VA��TA��A���A��7A�x�A�`BA�E�A��A��A��HA���A��RA���A��A�p�A�`BA�XA�G�A�?}A�33A��A�A��yA���A��^A���A�v�A�bNA�M�A�+A��;A���A�r�A�jA�S�A�E�A�7LA�$�A��A�JA���A���A��A��A��yA��HA��RA�ZA��wA�+A��A��jA��PA�~�A�XA�+A��A��A�  A��A�A���A��hA��A�n�A�G�A�(�A�VA��RA��7A�ZA�XA�C�A�1'A�"�A��A�oA�bA�VA�JA�1A�1A�A��A��HA��A���A���A�bNA�bA�hsA�  A��A��A�(�A�ƨA�;dA�%A��\A�{A���A�I�A���A���A��PG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                          A�C�A�;dA�?}A�=qA�;dA�9XA�5?A�/A�+A�&�A�$�A��A��A��A�ĜA�z�A�K�A�5?A�bA�\)A�oA��
A���A���A��
A��
A��
A��A���A���A���A��
A��/A��;A��TA��HA��#Aˡ�A˃A�dZA�ZA�G�A��TA�G�A��Aɩ�A�7LA�VA�ĜA�%AǅA���A�S�AŇ+A��HA�33A�&�A���A�1A��`A���A�"�A��
A�$�A�VA�jA�bNA���A�ƨA� �A��A��^A�VA���A�M�A��DA��`A���A��9A���A���A�bNA�C�A��A��HA��-A�&�A��A�hsA�ƨA�M�A�bA��A���A��yA�E�A��DA�`BA��A��A���A��RA�{A�l�A��A��FA�$�A`BA}�Ay�wAv��Asx�Ar�jAq��Ap��Al  Ah�Af1AbȴA^bNAZ$�AXJAU�TAR�\AP�`ANJAI�;AIS�AH�yAE�mABVABM�AB9XAB  A@�+A=x�A:��A8{A7?}A6�A5A4��A2jA/�;A.��A-A,A�A*I�A)�PA(�`A($�A'��A&�9A%��A%
=A$ĜA$-A#�hA#O�A!A jAhsA~�A-AO�AM�AO�A�DAv�A��A��A�jA7LAAffA��A��AVAbAXAJAO�A��Ap�A
��A
VA	�A	%A(�A`BAA�AȴA�A�`AM�A��A�A ��A �\A A�A @��@�+@���@��@���@�V@�S�@��@���@�o@��H@�-@���@�!@�~�@�^5@�j@��y@�hs@�@�+@��@�\@�n�@�^5@ꗍ@�R@�@�7L@�7L@��@�j@�9@�9@�@�r�@�I�@�9X@�9X@�1@�o@�@��;@�v�@���@�b@߮@�o@�M�@ݩ�@�V@���@�j@�
=@��@ش9@��;@�K�@��@���@�M�@���@�o@��H@��T@���@�O�@�;d@�-@���@ёh@Л�@�Z@�\)@Ͳ-@ͺ^@ͺ^@ͺ^@�r�@˶F@��@ʏ\@�=q@��#@�O�@ț�@��@�J@ʇ+@�n�@�5?@�@�7L@�x�@Ɂ@�O�@ȼj@�33@Ɵ�@�@�p�@Ĵ9@�I�@�  @�o@���@��@���@���@�/@��@�  @�A�@� �@�o@�V@���@��j@�(�@���@��@�o@��R@���@�ff@�V@�{@��@��@��@��@��P@��@��@��y@��y@���@���@�p�@��@��D@��m@�ƨ@��F@��P@��y@���@���@��h@�p�@�`B@��@���@�r�@�1'@���@�ƨ@�\)@�S�@�K�@�33@��@���@���@�v�@�=q@���@���@�p�@��j@��@��@��@�K�@�"�@��@�@��-@���@��7@�&�@��@���@���@��@�l�@�\)@�K�@��@�@��@�ȴ@���@�n�@�5?@��-@��@�X@�Ĝ@�j@�9X@��;@�l�@�33@�@��@���@�E�@�@�G�@���@���@�j@���@���@�C�@�
=@��!@�ff@��@�5?@�@��7@�O�@�G�@��@���@��@��u@��D@�r�@�1'@���@���@�S�@���@��+@�V@�$�@�J@��T@���@�?}@���@��/@��@�Q�@��
@��@�
=@��y@��R@��\@�M�@���@�x�@�O�@��@��@��@�Z@� �@��@���@�|�@�t�@�t�@�\)@�
=@���@�ff@�V@�=q@�{@���@��T@�hs@�%@��/@���@�Ĝ@��j@��D@�1@��
@��P@�C�@�"�@���@�ff@��@��@�O�@�&�@�V@���@��@��j@�Q�@���@��F@��P@�33@��y@���@�M�@��@���@�x�@�V@��u@��D@��@�z�@��@��@�j@�Z@�9X@��w@�;d@��\@�J@�@��@�O�@��@��/@���@��9@��@�j@�A�@��@�b@���@��@��y@�ȴ@��R@���@���@�ff@�E�@�@�G�@��@��@��@��@�V@�%@���@���@���@��@�r�@�9X@;d@~��@~�+@~$�@}�T@}@}��@}p�@}`B@}`B@}O�@}�@|��@|Z@{ƨ@{33@z�\@z^5@y��@y��@y��@y&�@x��@x�@w��@wl�@w\)@wK�@wK�@w�@v�+@v{@u��@u�-@u?}@t�/@t�D@s��@r��@r~�@q��@q�7@qG�@p��@p��@pQ�@o�@o��@n�@nE�@m@mp�@m`B@l�/@l�@l��@lz�@l(�@k��@j��@j�\@jn�@j�@i�@i��@i��@h�9@h1'@g��@g�w@g|�@g
=@f�R@fV@f@e�h@d�@dj@ct�@b�H@b~�@b�@a�@a��@a�7@aX@aG�@a7L@`�`@`�@`Q�@`b@_��@_�P@_+@^��@^E�@^{@]�T@]��@]�@]?}@]V@\�@\�@\�/@[�m@[dZ@Z�@Z~�@Z=q@Z-@Z�@Y��@X��@XĜ@XQ�@W�@W\)@V�@V{@Up�@UV@T��@T�D@T9X@S�F@St�@So@R�@R��@R��@R�@Q�^@QX@Q&�@Q%@P��@PĜ@P�u@Pr�@Pb@O��@Ol�@O+@N�@N��@NE�@N@M��@MO�@L�j@LZ@L�@K��@KS�@KS�@K33@K@J��@J~�@I�@Ihs@H��@HbN@HA�@Hb@G�P@G\)@G+@F��@F@Ep�@E/@D��@D�/@D�@Dj@C�m@Cƨ@C��@CC�@C@C@B�@B~�@BJ@A��@Ahs@A7L@A�@@ �@?��@?|�@?\)@?;d@?+@?�@>��@>@=�-@=�h@=/@<�@<z�@<1@;ƨ@;C�@;"�@:��@:�!@:M�@9��@9�#@9��@9��@9X@8��@7��@7�P@7l�@7\)@7\)@7
=@6��@6@5��@5/@4�/@41@3�F@3�@3dZ@3dZ@3dZ@3S�@2��@2�!@2�\@2=q@1�@17L@0��@0�`@0Ĝ@0��@0A�@0  @/�;@/��@/|�@.��@.ff@.{@-�@-�h@-�@,��@,��@,Z@,(�@,(�@,�@+t�@*��@*�@*J@)�@)�#@)��@)x�@)%@(�9@(�@( �@'�w@'�@'�P@'�P@';d@&�@&�R@&�+@&E�@&{@%�T@%�@%�h@$�/@$I�@$9X@$9X@$9X@$9X@$(�@$(�@$1@#�m@#ƨ@#��@#S�@#"�@"�H@"�\@"�\@"~�@"M�@"�@!�^@!X@ ��@ ��@ ��@  �@�;@��@|�@K�@�@��@V@5?@�@O�@�@��@��@�D@(�@1@�m@��@C�@@@�@��@^5@^5@=q@J@�@�#@��@��@x�@7L@%@��@��@�@bN@bN@b@�w@|�@;d@+@�@
=@��@�R@��@v�@V@5?@5?@5?@@��@�-@�-@��@/@�@z�@I�@��@�
@�F@t�@dZ@"�@�@�H@�H@��@�!@~�@=q@��@�@��@x�@X@&�@&�@%@Ĝ@��@bN@1'@ �@  @�@  @  @  @�@�@��@��@��@��@��@�P@+@
=@��@��@�y@�R@�+@ff@$�@@��@��@`B@O�@?}@V@V@�/@�A�O�A�S�A�=qA�;dA�?}A�;dA�9XA�?}A�A�A�?}A�;dA�?}A�;dA�=qA�?}A�;dA�7LA�9XA�9XA�5?A�;dA�7LA�/A�1'A�33A�-A�-A�/A�(�A�&�A�&�A�(�A�"�A�$�A��A��A� �A��A��A��A��A�oA��A��A��A��A��A��A�5?A�I�A�E�A�;dA���AυA�VA�VA��TAΛ�A�E�A�Aͧ�AͅA�|�A�hsA�VA�S�A�S�A�I�A�O�A�M�A�A�A�;dA�9XA�5?A�5?A�5?A�-A�/A�$�A�
=A�JA�JA���A��
ȂhA�ZA�9XA�1'A�+A� �A�"�A�$�A��A��A��A��;A��A��A���A���A���A��
A���A���A���A��
A���A���A��
A���A���A���A��
A���A���A��A���A���A��A��
A���A��A��A���A��
A��A��A���A��
A��#A��
A���A��#A��A��
A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A��
A��A��A��/A��/A��/A��/A��TA��;A��/A��HA��TA��/A��HA��`A��TA��;A��TA��`A��HA��TA��TA��HA��;A��HA��TA��HA��yA��A��A���A˰!A˰!AˮAˡ�A˙�A˟�A˙�A˕�AˑhAˏ\A˃A�r�A�l�A�hsA�ffA�bNA�bNA�ffA�`BA�\)A�\)A�^5A�VA�Q�A�O�A�M�A�G�A�E�A�G�A�C�A�=qA�9XA�"�A��`Aʧ�Aʏ\A�|�A�hsA�Q�A�;dA�1'A�&�A�oA�
=A���A��A��A��
A���AɸRAɣ�Aɧ�Aɥ�Aɗ�AɁA�`BA�1'A��A��A�{A�oA�VA�JA�VA�bA�JA���A���A��mAȇ+A�S�A�=qA�/A�"�A���A���A�ĜAǺ^AǃA�v�A�l�A�jA�`BA��A���A��A��#A���Aƥ�A�x�A�ffA�E�A�(�A���A���Ať�AŋDA�x�A�E�A�7LA�7LA� �A���A��TA��
Aĉ7A�ZA¬A�~�A�7LA�A��
A���A�hsA�S�A�{A��yA��uA�{A��-A��+A�XA�=qA�-A�"�A��A�JA��A�ȴA�x�A�;dA�/A�+A�(�A��A��A� �A��A�{A�1A���A���A���A��A��A��A��A��A��A��yA��TA��TA��`A��TA��#A��A��#A��
A���A���A���A�ƨA��wA���A��-A���A���A�z�A�^5A�?}A�"�A��A�l�A��-A�(�A��;A���A�\)A�K�A�G�A�A�A�%A�A���A���A���A���A��mA��
A�~�A�VA�=qA�9XA�&�A�"�A�$�A�&�A�"�A��A� �A��A��A�{A�1A���A��+A�M�A��A�A�  A���A��#A��;A�ȴA��^A���A��7A��A�\)A�-A��9A��TA�^5A�ȴA��\A���A��;A��/A���A���A���A���A���A�ƨA��wA��A���A��\A��A�r�A�\)A�K�A�{A�z�A���A� �A���A�?}A���A��hA�jA�VA�/A�JA���A���A���A�n�A�JA�ĜA���A�p�A�Q�A��A�oA�
=A��A�ĜA�A��jA��!A��9A���A���A��7A��A�hsA�ZA�K�A�O�A�M�A�K�A�?}A��A�1A��A��mA���A���A��-A���A�ffA�VA��uA���A�r�A�VA�1A�
=A��A��A��A��wA���A���A�~�A�hsA�r�A�ffA�l�A�-A�JA�A��mA��TA��HA��;A���A���A�hsA���A��
A���A���A��PA�~�A�^5A�C�A�+A�
=A���A��#A��A�jA�O�A�9XA��A���A��TA���A���A��uA��PA��A�r�A�1'A���A��A���A��jA��wA��wA��RA��hA�A�A���A��wA���A�VA�|�A�E�A�bA��HA���A��PA�7LA��`A�ƨA��RA��PA�1'A�VA��HA���A��^A���A��PA�p�A�S�A�/A�bA�A��A���A��^A��-A���A��+A�bNA���A���A���A��A�+A��/A�ĜA��wA��^A��9A��!A��-A��!A��A���A���A��DA��A�r�A�\)A�=qA��A��yA��/A���A��A��A�33A��yA���A��A�x�A��RA�A�A��A���A��wA���A��7A�z�A�v�A�t�A�I�A�%A�x�A�"�A��A��;A���A�ȴA�A���A�r�A�A�A�/A��A�%A�  A���A��jA��A�\)A�;dA�+A�"�A��A�bA�  A��A��TA��A�ƨA��!A���A�|�A�ffA�\)A�9XA�(�A� �A��A�VA��TA��A���A��7A�x�A�`BA�E�A��A��A��HA���A��RA���A��A�p�A�`BA�XA�G�A�?}A�33A��A�A��yA���A��^A���A�v�A�bNA�M�A�+A��;A���A�r�A�jA�S�A�E�A�7LA�$�A��A�JA���A���A��A��A��yA��HA��RA�ZA��wA�+A��A��jA��PA�~�A�XA�+A��A��A�  A��A�A���A��hA��A�n�A�G�A�(�A�VA��RA��7A�ZA�XA�C�A�1'A�"�A��A�oA�bA�VA�JA�1A�1A�A��A��HA��A���A���A�bNA�bA�hsA�  A��A��A�(�A�ƨA�;dA�%A��\A�{A���A�I�A���A���A��PG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
�-B
��B
��B
��B
�-B
��B
�B
��B
��B
�<B
�B
�,B
�]Bh�B�"B�~B�xB�=B��B��B��B��B��B�'B��B��B�tB��B��B�[B��B�B�'BƨB�B�BN�BX�Ba�Bd&Be�BrB��B��B�	B��B��BĜB��B�B�&B�BB�B�B�QB�B��BԕBбBɆB�wB��B��B�jB�zB��B�=B��B�7B�Bw�BsMBm�BX�BR BT�BK)B?�B0�B 'B	B�;B�HB֡B��B�0B�XB~(BcTBVB-wB&�BCB�BMB
��B
�QB
�5B
�HB
��B
��B
}�B
}�B
ffB
C-B
8�B
4nB
,B
B
�B	�B	�B	ޞB	��B	�'B	��B	��B	�:B	s�B	^jB	R�B	B'B	/OB	"�B	eB	�B	�B	  B	 �B��B��B�B��B��B��BیB҉B�pB��BȴB�?B�EB�OB�^B��B�[B�UB��B��B��B��B�[B��B��B̘B��B�B�6B�,B�pB��BǮBĜBÖB�qB�B��B�B�B�4B�hB�~B�$B��B�B��B�SB��B�	B�nB�RB�wB�LB�^B�^B��B�HB�}B��B�OB�wBǮB�3B��BÖB�3B�EBȀB�XB�6B��B��B�EBیB��B�B�B�
B��B�B�DB�5B��B�GB�AB�;B��B�TB�%B�TB��B�>B��B	�B	�B	�B	
rB	�B	@B	�B	xB	$B	)_B	)*B	)*B	)*B	*�B	*�B	*eB	)�B	)�B	)�B	'RB	)�B	'RB	.}B	-CB	,B	,B	,B	-�B	.B	/OB	5�B	@�B	C�B	I�B	K^B	K�B	J�B	J�B	OB	VmB	Y�B	c�B	bB	a|B	a�B	`�B	]dB	\)B	[�B	\�B	]�B	`vB	^�B	aHB	c�B	g�B	g�B	g�B	h>B	h�B	iDB	kQB	n�B	qB	v+B	~(B	�GB	�GB	�GB	�;B	��B	��B	��B	�~B	��B	�MB	�B	��B	�B	��B	��B	��B	��B	�SB	��B	��B	��B	�kB	�B	�=B	��B	��B	�B	�eB	�B	��B	��B	��B	�kB	�B	��B	�!B	�UB	�!B	��B	�zB	��B	��B	��B	��B	�B	��B	�B	ǮB	ȴB	�B	��B	�vB	� B	��B	�&B	�[B	��B	֡B	�yB	�EB	�yB	خB	�B	�KB	�KB	��B	چB	ںB	��B	�]B	�]B	�]B	ܒB	��B	��B	ݘB	��B	ޞB	�;B	�pB	�B	�NB	�HB	�|B	� B	�B	�TB	�B	��B	��B	�B	��B	�8B	�yB	�B	�B	�B	�B	��B	�WB	�)B	�]B	��B	�cB	��B	��B	��B	�B	�B	��B	�B	�B	�B	��B	�%B	�%B	��B	��B	��B	��B	�B	�8B	�8B	�lB	��B	�B	�DB	�JB	�DB	�JB	��B	��B	��B	�.B
  B
 4B
 4B
 4B
B
 �B
 �B
 iB
 iB
 �B
 �B
 �B
�B
�B
B
�B
{B
{B
�B
�B
�B
B
B
�B
YB
_B
�B
	lB
	lB

	B

=B

�B
�B
B
�B
B
�B
B
�B
�B
�B
(B
�B
�B
�B
�B
�B
�B
hB
�B
B
�B
�B
B
B
B
�B
�B
�B
{B
�B
MB
B
�B
�B
�B
�B
_B
�B
�B
�B
7B
7B
kB
kB
�B
�B
xB
xB
�B
~B
~B
OB
�B
!B
�B
 'B
!bB
"hB
"hB
"hB
"�B
"hB
"hB
"hB
"4B
"hB
#B
#nB
#B
"�B
#B
"�B
"hB
"�B
#�B
#�B
$@B
%B
&LB
&�B
'B
'B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)_B
)�B
*0B
+6B
*�B
*�B
*�B
+B
+B
*�B
+B
+6B
+�B
,=B
-CB
-�B
.�B
.�B
/OB
/OB
/�B
0UB
0�B
0�B
0�B
1[B
2-B
49B
4�B
5B
5�B
6FB
6zB
6zB
6zB
6zB
7B
7�B
7LB
7�B
8RB
8�B
8�B
8�B
8�B
8�B
9XB
9XB
9�B
9�B
9�B
:*B
9�B
;dB
;�B
;�B
<6B
<6B
<6B
<�B
<�B
=B
=<B
=qB
=�B
>BB
>�B
>�B
>�B
?�B
?B
?HB
?�B
?�B
@OB
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B'B
B'B
B'B
B[B
B�B
C-B
B�B
C-B
D3B
C�B
EB
D�B
E�B
EmB
E�B
E�B
E�B
E�B
E�B
FB
F�B
GB
GB
GEB
G�B
G�B
HB
H�B
H�B
IB
IB
IB
I�B
I�B
I�B
I�B
I�B
I�B
JXB
J�B
J�B
K^B
K�B
K^B
K^B
K�B
L�B
LdB
L�B
M6B
M6B
M�B
N<B
N�B
N�B
OB
OB
OvB
O�B
PB
P}B
PHB
P}B
P}B
QB
QB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
RTB
R�B
R�B
R�B
S&B
S[B
S�B
S�B
S�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
VB
VB
V�B
W?B
W�B
W�B
W�B
XB
XyB
X�B
XyB
X�B
Y�B
ZB
ZB
ZQB
ZQB
Z�B
Z�B
[#B
[#B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
^�B
^jB
^5B
^jB
^�B
^�B
^jB
^jB
_�B
_�B
_�B
`BB
`BB
`�B
aB
aHB
a�B
a�B
a�B
a�B
b�B
b�B
b�B
b�B
b�B
b�B
cTB
dZB
dZB
dZB
dZB
dZB
d�B
e`B
e�B
f2B
ffB
f�B
g8B
g8B
g�B
g�B
g�B
gmB
gmB
h>B
g�B
h
B
h>B
hsB
iB
iB
iB
iB
iDB
i�B
i�B
i�B
iyB
i�B
jB
kB
kB
kB
kQB
k�B
k�B
k�B
lWB
lWB
l"B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
n/B
n�B
o5B
o5B
o5B
o�B
o�B
o�B
pB
pB
q�B
qB
qAB
p�B
qB
p�B
p�B
qAB
q�B
r�B
r|B
rB
rGB
rB
rB
rGB
rB
rGB
r|B
r|B
r|B
sMB
sMB
s�B
s�B
s�B
s�B
s�B
s�B
tTB
t�B
uZB
u�B
v+B
v�B
v`B
v�B
v�B
v�B
v�B
w2B
w2B
w2B
w�B
w�B
wfB
wfB
w�B
x8B
x�B
y>B
y>B
y>B
zB
zB
y�B
zB
zDB
zDB
zDB
z�B
z�B
z�B
z�B
z�B
{B
{B
{JB
{JB
{B
{�B
|B
|PB
|B
|�B
|�B
|�B
|�B
|PB
|�B
|�B
|�B
}"B
}"B
}VB
}VB
}�B
}VB
}�B
}�B
}�B
}�B
}�B
~(B
~�B
~�B
.B
~�B
�B
�B
�B
�4B
�4B
��B
��B
��B
��B
��B
��B
��B
�B
�;B
�;B
�oB
�uB
�uB
��B
��B
��B
�B
�B
��B
�{B
�{B
�{B
��B
�{B
��B
��B
��B
��B
�B
��B
��B
�MB
��B
�MB
�SB
�B
�B
�B
�B
��B
��B
��B
�YB
�%B
��B
��B
�+B
�+B
��B
�_B
�+B
��B
��B
�-B
��B
�B
��B
��B
��B
��B
�[B
��B
��B
�hB
��B
��B
��B
�UB
��B
��B
��B
��B
��B
��B
�aB
��B
��B
��B
�B
�B
�tB
�B
�*B
�dB
�dB
��B
�<B
��B
�aB
�UB
ƨB
�RB
˒B
̘B
�B
�B
уB
��B
�EB
چB
��B
�B
��B
�MB �B(XB9�BHB]�BhsByrB~�B��B��B��B�7B��B��B��B��B��B��B�B�hB�B�xB��B��B�	B�~B��B�DB��B�fB�+B�B��B��B�B�~B��B�PB�\B�VB�PB��B�B�"B��B��B�"B��B��B��B� B�FB��B�MB��B��B��B��B�VB�-B�VB��B�-B��B��B�\B�bB��B�\B��B�'B�\B�4B��B��B��B��B��B�4B��B��B�nB��B�FB��B�*B��B�qB��B��B�=B�6B�6B��B��B��B��B�-B��B�B��B��B�hB��B��B��B��B��B�0B��B�'B��B�UB�[B� B��B��B�[BÖB��B��B�B�zB�#B�pB�XBʌB�dB�6BϫB�WB��B��BoB'�BE9BHKBIRBOvBR BPBQ�BR�BS�BU2BZQB_B_�B`�Ba|Bb�BbNB`�Bb�Bd�Bc�Bb�Be�Be`Be,Be,BgmBgBd�Bf2Bf�Bg8Bk�BsMB{JBy>B|�B{B~�B�GB��B�%B��B��B��B��B��B��B��B��B�IB�=B��B�B�:B�eB��B��B�B��B�B��B��B��B��B�XB��B�jB�<B�sBуB֡B֡B�yB�5B�NB�BޞB�B��B��BݘB�B��B�,B� B�B�B�B�B�pB�B��B�/B�BߤB��BٴB�B�B�TB��B�B�B�B�lB�%B�B�QB�B��B� B�|B�ZB�BB�|B��B�B��B��B�HB�/B�QB��BخB�B�sB��BݘB��B��B�yB��B�gBרBרB��B��B�mB�aB�gB҉B� B�&B�[B�TBуB�&B�NBҽB�BуB��B�vB��BϫB�6B�dB��B͟B��BʌBʌBȴBȀB��BǮB�B��B�[B�UB��B��B�RB��B�B�EBȀB�B�'B��B�B��B��B�?B��B�nB��B��B��B��B��B��B��B��B�qB�B�dB��B��B�B�B��B�B��BBŢB��B��B��B��B��B�*B��B��B�FB��B��B�B�B��B��B��B�wB��B��B�hB��B�B�B�~B�CB��B�xB�IB�qB��B�CB��B�+B�1B�+B�MB�IB��B�kB�	B�YB�tB��B��B��B��B��B�B�YB�~B�fB�7B�B��B�B� B{B��Bt�Bz�BzxB{JBpBx�Bu%Bs�Bt�Bv�Bx8BuZBs�Bs�Bv�Bo�BpoBqABs�Bl�BncBjBl"BkBf�Bd�Bc�Bm�BQB�PBN<BHBYBU2BO�BPBQNBQ�B^�BPHBN�BT�BT�BOBR�BO�BV�BR�BVmBS&BU2BNBPBYBK�BRTB`vBK^BMjBD3BFBB[BC-BB'BD�BC�B?�BA�BCaB>�B;�B:*B<6B5B5tB4�B1�B+�B+B,�B.�B-�B.B�B~B�B�B�BB�B($BB4B�B�BoB��B%B�"B��B�cB�B�B�B�iB�2B�|B�GB�B�>B�2B��B��B�B�,B�,B�B��BޞB�;BںB��B�B�EBٴB�BуB�B��B��B��B�mB��B�gBĜB��B�UB��B�OB��B��B� B��B�OB�BB�HB��B��B��B��B�aB�aB�$B�B�eB�}B�B��B��B�B�GB�B��B|�By	Bv`Bw�B{�B~�BzDBv�Bd�Bb�B_;B^�B^5BaBaB\)BWsBV�BRTBNpB`vBi�BT�B7B33B.}B-�B.IB-CB,�B*�B)*B)�B*�B)*B*�B(XB$B#nB&�B!B~BB�B!�B"�B�BMB$B�BFB�B�B"B"B
�BxB�B�B�B�BSBGB�B�B �B �B
�cB
�cB
��B
�"B
��B
�2B
�B
��B
��B
�]B
��B
�B
�B
��B
��B
� B
� B
�|B
ޞB
�;B
ܒB
�QB
��B
�B
�ZB
�B
�sB
��B
�^B
�nB
�=B
�}B
��B
�@B
�hB
��B
��B
�B
��B
�7B
�_B
��B
�OB
�MB
�MB
��B
��B
�;B
��B
�;B
�AB
}�B
�B
~�B
~(B
|�B
|�B
{�B
zxB
z�B
{JB
yrB
v�B
xlB
��B
~�B
��B
�CB
q�B
n�B
r�B
d�B
r�B
`�B
Y�B
\�B
d�B
M�B
HKB
JXB
H�B
C�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                          B
�B
�}B
�IB
�B
�IB
�}B
�OB
�[B
��B
�B
��B
�[B
�|B
��Bd�B�rB��B��B��B��B�IB�IB�B��B�wB�IB�OB��B��B��B��B� B�dB�wB��B�QBGBJ�BU2B^B`vBbNBncB}�B�B�YB�B�B��B�EB��B�vBܒB�QB��B�B�B�8B��B�B��B��B�B�#B��B��B�B��B��B��B}VBs�Bo�BjBT�BNpBQBGyB<6B-BwBSB�BݘB��B�B��B��BzxB_�BRTB)�B#9B�BIB �B
��B
�B
څB
��B
��B
�CB
zDB
zDB
b�B
?}B
5?B
0�B
(XB
bB	��B	�fB	��B	��B	� B	�wB	��B	�B	��B	p;B	Z�B	OB	>wB	+�B	!B	�B	
	B	 �B�PB��B�JB�B��B�8B�B�B��B��B��B�KB�BBÕB��B��B��B��B��B�)B�0B��B�<B��B��B�9B��B�B�QBɆB�|B��B�KB��B��B��B��B�mB�-B�RB�UB��B��B��B�tB�4B�nB�B��B��B�YB��B��B��B��B��B��B�B��B��B�B��B��B��B��B�B��B��BÕB��BƨBɆB�5B�BԕB��B�#B��B��B�ZB�2B��B�B�B�DB�B�B�B�B�B�uB�B��B��B�B��B	@B	B	�B	IB	�B	LB	�B	 [B	%�B	%zB	%zB	%zB	'B	'B	&�B	&LB	&LB	&LB	#�B	%�B	#�B	*�B	)�B	(XB	(XB	(XB	)�B	*dB	+�B	1�B	=B	@NB	F
B	G�B	G�B	GB	GB	K^B	R�B	VB	`AB	^iB	]�B	^B	]/B	Y�B	XyB	XB	YB	ZB	\�B	Z�B	]�B	`AB	c�B	d%B	c�B	d�B	d�B	e�B	g�B	j�B	m]B	r{B	zxB	�B	�B	�B	}�B	}"B	��B	��B	��B	�!B	��B	�hB	��B	�hB	�4B	�:B	�FB	�B	��B	��B	�FB	�B	��B	�kB	��B	��B	��B	�nB	��B	�RB	��B	��B	�B	��B	�XB	�B	�qB	��B	�qB	�IB	��B	��B	�#B	�6B	�B	�[B	�3B	�gB	��B	�B	�QB	�)B	��B	�pB	�BB	�vB	ϫB	�BB	��B	��B	ԕB	��B	��B	�`B	՛B	՛B	�>B	��B	�
B	�B	حB	حB	حB	��B	�B	�B	��B	�B	��B	ۋB	��B	�]B	ޞB	ݘB	��B	�pB	��B	ߤB	�B	�B	�NB	��B	�B	�B	��B	�lB	�lB	��B	�
B	�>B	�B	�yB	�B	�B	�B	�B	�"B	�(B	�iB	� B	�5B	�B	� B	� B	�;B	�uB	�uB	��B	��B	�B	��B	�SB	�B	�B	��B	��B	�`B	��B	��B	��B	��B	�B	��B	�DB	�~B	�PB	��B	��B	��B	�VB	�"B	��B	��B	��B	��B	�"B	��B	��B	��B	�bB	�.B	��B	��B
  B
B
:B
oB
oB
�B
�B
�B
B
�B
�B
YB
�B
�B
1B
eB
1B
eB
	7B
	kB
	�B

=B
B
xB
�B
�B
�B
IB
B
!B
�B
�B
VB
�B
'B
\B
bB
hB
4B
4B
4B
�B
�B
�B
nB
�B
FB
FB
B
�B
LB
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
qB
CB
wB
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
UB
�B
UB
!B
UB
�B
�B
!B
�B
�B
 �B
!bB
"�B
#9B
#nB
#nB
$B
$�B
$�B
%B
%B
%B
%B
%�B
%�B
&�B
'�B
&�B
'B
'B
'RB
'RB
'B
'RB
'�B
'�B
(�B
)�B
*0B
+6B
+B
+�B
+�B
,<B
,�B
,�B
-BB
-BB
-�B
.}B
0�B
0�B
1[B
2-B
2�B
2�B
2�B
2�B
2�B
3gB
3�B
3�B
3�B
4�B
5?B
5?B
5?B
5B
5?B
5�B
5�B
5�B
5�B
6EB
6zB
6EB
7�B
7�B
7�B
8�B
8�B
8�B
9#B
9#B
9XB
9�B
9�B
:)B
:�B
;0B
;0B
:�B
<B
;dB
;�B
<B
<6B
<�B
<�B
=B
=B
=<B
=<B
=<B
=<B
=�B
>BB
>wB
>wB
>wB
>�B
>�B
?}B
?HB
?}B
@�B
@B
AUB
A B
A�B
A�B
A�B
A�B
B&B
B&B
B&B
B[B
B�B
CaB
CaB
C�B
C�B
C�B
DgB
EB
E9B
EmB
EmB
EmB
E�B
F
B
F
B
E�B
E�B
E�B
F�B
GB
GEB
G�B
G�B
G�B
G�B
G�B
IB
H�B
H�B
I�B
I�B
J#B
J�B
K)B
K)B
K^B
K^B
K�B
L/B
LdB
L�B
L�B
L�B
L�B
MjB
MjB
M�B
M�B
NB
NB
NB
N<B
NB
N�B
N�B
N�B
N�B
OBB
OvB
O�B
O�B
O�B
PHB
P�B
QB
QB
Q�B
Q�B
Q�B
Q�B
Q�B
RTB
RTB
R�B
S�B
T,B
T,B
T,B
T`B
T�B
T�B
T�B
T�B
V8B
VmB
VmB
V�B
V�B
V�B
W
B
WsB
WsB
W�B
XEB
XEB
XB
W�B
X�B
X�B
YB
YB
YKB
YB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
\)B
[�B
[�B
\�B
\�B
]/B
]cB
]�B
^B
^5B
^5B
^5B
^�B
^�B
^�B
^�B
_B
_B
_�B
`�B
`�B
`�B
`�B
`�B
`�B
a�B
bB
b�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d�B
d%B
dZB
d�B
d�B
e`B
e`B
e`B
e`B
e�B
e�B
e�B
e�B
e�B
f2B
f�B
glB
glB
glB
g�B
g�B
h>B
h>B
h�B
h�B
hrB
h>B
iDB
i�B
jJB
jJB
jJB
jJB
jB
kB
k�B
k�B
k�B
k�B
k�B
k�B
lWB
lWB
m�B
m]B
m�B
m(B
m]B
m(B
m(B
m�B
n.B
o5B
n�B
ncB
n�B
ncB
ncB
n�B
ncB
n�B
n�B
n�B
n�B
o�B
o�B
o�B
pB
o�B
o�B
pB
p;B
p�B
qB
q�B
rB
r{B
sB
r�B
r�B
sMB
r�B
sB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
u%B
u�B
u�B
u�B
v`B
v`B
v+B
v`B
v�B
v�B
v�B
v�B
w1B
w1B
w1B
w1B
wfB
wfB
w�B
w�B
w�B
xB
xlB
x�B
xlB
y	B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
yrB
yrB
y�B
y�B
y�B
y�B
y�B
zDB
zDB
zDB
zDB
zxB
{B
{B
{~B
{JB
{�B
{�B
|B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}VB
}�B
}�B
}�B
~�B
~�B
~�B
~�B
.B
bB
bB
� B
�B
�B
�B
� B
�B
�4B
� B
�4B
�4B
�iB
��B
��B
��B
��B
��B
��B
�oB
�oB
�oB
�oB
��B
��B
�B
��B
�uB
��B
�B
�{B
�{B
�GB
��B
�{B
�B
�B
�}B
�B
�aB
��B
�IB
��B
�B
��B
�BB
�OB
��B
�BB
�B
�B
��B
�B
�OB
�B
�B
��B
�B
��B
��B
��B
� B
�aB
�gB
��B
�mB
�zB
��B
��B
��B
��B
�<B
��B
��B
��B
ŢB
��B
��B
�jB
�jB
��B
� B
ԕB
��B
�>B
�WB
�>B
�B
��B$�B6EBDgBZBd�Bu�B{JB��B��B�=B��B�	B�CB��B�7B�B��B�eB��B�eB��B�B��B�YB��B�1B��B��B��B�{B�_B��B�GB�SB��B�1B��B��B��B��B��B�RB�rB��B�B�rB��B�B�B�PB��B�B��B��B�7B��B�=B��B�}B��B�7B�}B�B�B��B��B�CB��B��B�wB��B��B�B�B�B��B�!B��B��B��B��B��B��B��B�zB��B��B��B��B��B��B��B��B��B��B�B�}B�IB�UB� B��B��B��B� B�3B�)B��B��B�BB�wB�6B��B��B�pB�BB�NB��B��B�NB�,B�aB��B�sB��BƨB��BȴBɆB��BקB�B�GB��B$BA�BD�BE�BK�BNpBLdBNBOBBPBQ�BV�B[WB\)B]/B]�B^�B^�B]/B_;B`�B`B^�BbBa�Ba|Ba|Bc�BcTBaBb�Bb�Bc�Bh
Bo�Bw�Bu�Bx�Bw�B{JB�B~�B�uB��B��B�=B��B��B��B�B�B��B��B��B�eB��B��B�OB�'B�gB��B�gB��B�B�?B��B��B��B��B��B��B��B��B��B��BڅBޞB�]B��B�`B�)B�)B��B��B�(B�|B�pB��B�B�B�`B��B�B�>B�B��B��B�KB�B�iB�BߤB�B��B�TB�B��B�uB��B�B�fB�%B�pB��B�BܒB��B�2B��B�2B�BݘB�B֡B�B��B�`B��B�B��B�AB�2B��B�BѷB��B��B�HB�NBҽBбBѷB��B�pB�vBϫBΤB��B�vB͞B�B�jB��B�/B��B�5B��BɆBȴB�)B��B�B��B��B�B��B�3B��B�[B� B��B��B�BB�5BŢB�#B�^BÕB��B�^B�wB�3B�UB�B��B��B� B��B��B�B��B�#B�NB�B�B��B��B�RB��B�#B�)B�RB�RB�#B�XB��B��B��B��B��B�KB��B�?B�zB��B� B��B��B�<B�dB�gB�0B�?B�OB��B�B��B��B��B�_B�kB��B��B�$B��B��B��B�*B��B��B�{B��B�{B��B��B�*B��B�YB��B��B��B�B��B�B��B�oB��B��B��B��B�iB��BbB|PBwfB~�Bp�Bv�Bv�Bw�BlWBu%BquBp;BqBsMBt�Bq�Bo�BpBr�Bl"Bl�Bm�Bp;Bh�Bj�BffBhrBglBb�B`�B`ABi�BMjB��BJ�BDgBUgBQ�BL/BLdBM�BM�B[#BL�BK)BQNBQBK^BOBBL/BS&BN�BR�BOvBQ�BJWBLdBU�BHKBN�B\�BG�BI�B@�BB[B>�B?}B>wBA B@NB<6B>BB?�B:�B7�B6zB8�B1[B1�B0�B-�B($B'RB(�B+6B*0B*dBB�BCB0B�BkB$B$tB_B�B�B@B�B��BuB�rB�B��B 4B�`B��B�B�B��B�B��B�B�B�B�B��B�|B�|B�B�B��BۋB�
B�>B��BԕB�B�B��B�dB�HB�,B�B��B�B��B��B�B��B�B��B��B��B�pB�0B��B��B��B��B�0B��B�B��B��B�tB�^B��B��B�kB��B�FB�SB�BbB�4Bx�BuYBr�Bs�BxB{JBv�BsB`�B_B[�BZ�BZ�B]cB]cBXyBS�BR�BN�BJ�B\�Be�BP�B3gB/�B*�B)�B*�B)�B(�B'B%zB%�B&�B%zB'B$�B [B�B"�BqB�BeB=BB�BFB�BtB�B�B:BB
rB
rB+B�B
=B1B@B 4B�B
��B
�.B 4B
��B
�"B
��B
��B
��B
�rB
�%B
�B
�`B
�+B
�+B
�B
�,B
��B
�`B
�B
�NB
�pB
�pB
��B
��B
ۋB
��B
֡B
�2B
�QB
�B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�UB
�$B
��B
��B
��B
��B
��B
��B
�B
��B
}�B
�GB
}�B
~�B
zB
{�B
z�B
zxB
y	B
y	B
xB
v�B
w1B
w�B
u�B
sB
t�B
~�B
{B
.B
��B
m�B
kB
o B
aB
o B
\�B
VB
YKB
aGB
I�B
D�B
F�B
EB
@G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721224955                            20230721224955AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122495520230721224955  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122495520230721224955QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122495520230721224955QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               