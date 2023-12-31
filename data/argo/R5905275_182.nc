CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:47Z creation      
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
_FillValue                 �  [x   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �x   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �|   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �P   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20230426223247  20230426223247  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @��So�.@��So�.11  @��8�0@��8�0@+��{��0@+��{��0�cF�E���cF�E��11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  AA  ?�  @�\@B�\@}p�@��R@��R@�  A   A\)A   A+�A?\)A`��A���A��A��A�\)A��A�  A�Q�A��B   B  B�
B�B   B((�B0  B7�
B@  BH  BP  BX(�B`  Bg�
Bo�
Bxz�B�  B�  B��B��B��B�  B�  B��B�  B�  B�  B�  B�  B�{B�  B�  B�  B��B�  B�{B�  B�  B��B�  B�{B�  B��B��B�{B�  B��B��C   C
=C  C��C
=C
  C��C  C  C  C  C
=C
=C
=C
=C  C��C!��C#��C&  C(
=C*
=C,  C-��C0  C1��C3��C5��C8  C9��C;�C=�C?�CA��CD  CF
=CH  CJ  CL
=CN  CO��CR
=CT
=CV{CX{CZ
=C[��C^
=C`
=Cb
=Cd  Ce��Ch  Cj  Cl
=Cn
=Cp  Cr  Ct  Cv  Cx  Cz  C|  C~
=C�C�C�  C�  C�  C�  C���C���C���C���C�C�  C���C�  C�C�  C�  C�
=C�
=C���C���C�  C�
=C�C�C�  C�  C�
=C�
=C�C�
=C�C�  C�C�C�  C�
=C�  C���C�C�  C���C���C�  C�  C�  C���C���C���C�  C�  C���C�C�  C�  C���C���C�  C�  C�C�  C�  C�C���C�  C�  C�C�C�  C���C���C���C�C�C�C�  C�  C�C�C���C���C���C���C�  C�
=C�  C���C���C�  C�C�C�C�C�C�C�  C�C���C��C���C���C���C���C�  C�\C�C���C���C�  C�
=C�
=C�
=C�C�  C���C���C���C���C���C���C�  C�
=C�C�  C���C���C��C���D �D ��D  D� D  D� D  D}qD��D� D  D� D�D� D  D}qD�qD}qD�qD	z�D	��D
}qD�D� D�qD}qD  D�D  D}qD�D��D��D}qD�D� D�qDz�D  D��D��D}qD  D��DD�D�D� D�qD� D  Dz�D�qD� D�qD}qD�D��D�D��D�qD� D�D� D�qD }qD!  D!� D"  D"� D#  D#}qD$  D$��D%  D%� D&  D&� D'�D'�D(  D(� D)�D)� D)�qD*� D+  D+��D,  D,}qD,�qD-� D.  D.� D/�D/��D0�D0}qD1  D1� D2  D2� D3  D3� D4  D4}qD4�qD5� D6  D6� D6�qD7}qD7�qD8��D9�D9��D:�D:��D;  D;� D<�D<� D=  D=��D>  D>� D?  D?��D?�qD@� DA  DA��DB  DB� DC�DC��DD�DD� DE  DE}qDE�qDF� DF�qDGz�DG�qDH� DI�DI��DJ  DJ� DK�DK��DL�DL� DL�qDM}qDN�DN��DO  DO}qDO�qDP� DQ�DQ� DQ�qDR� DS  DS}qDT  DT� DT�qDU� DV  DV� DW  DW� DX  DX� DY  DY}qDY�qDZ}qDZ�qD[}qD[�qD\��D]�D]��D^�D^}qD^�qD_� D`  D`��Da�Da��Da�qDb}qDb�qDc� Dd  Dd� De  De��Df  Df� Dg  Dg}qDg�qDh��Di  Di}qDj  Dj��Dk�Dk��Dl  Dl}qDl�qDm}qDm�qDn}qDo�Do��Do�qDp� Dq�Dq� Dq�qDr}qDs  Ds� Dt  Dt��Dt�qDuz�Du��Dv}qDv�qDw� Dw�qDx� DyDy��Dz  Dz� Dz�qD{}qD|  D|� D}  D}� D~  D~� D~��D}qD�HD�AHD���D��HD���D�>�D�~�D���D�  D�@ D��HD���D�  D�AHD�� D��HD��D�AHD��HD�� D�  D�AHD��HD��HD���D�=qD�~�D��HD�HD�B�D�� D���D�HD�@ D�~�D��HD�HD�>�D�~�D�D��D�AHD��HD��HD�HD�>�D�� D�� D��qD�>�D��HD�D�HD�@ D�� D��HD�HD�AHD�� D�� D��qD�>�D�~�D���D���D�>�D��HD��HD���D�>�D�� D�� D�  D�AHD�� D���D�HD�@ D�� D���D��qD�@ D��HD���D���D�@ D�~�D��qD�  D�@ D�~�D�� D�  D�AHD��HD�� D���D�@ D��HD�� D�  D�B�D��HD�D�  D�=qD�� D�D��D�@ D�� D�D�HD�@ D�� D�� D���D�>�D�}qD�� D�HD�@ D�� D���D�  D�AHD�~�D���D�  D�B�D��HD�� D�HD�AHD�� D�� D�  D�>�D�~�D�� D�HD�@ D�� D�� D�HD�B�D��HD��HD�HD�AHD��HD��HD�  D�@ D��HD��HD��D�@ D�~�D�� D�HD�B�D��HD���D�  D�@ D�~�D��HD�  D�@ D�~�D���D�HD�B�D�� D��HD�HD�@ D��HD�D�  D�@ D�� D�� D�HD�@ D�� D�� D���D�@ D�� D�� D�  D�@ D��HD��HD�  D�AHD��HD�� D���D�>�D�~�D��HD�HD�@ D�~�D�� D�HD�AHD��HD��HD�  D�AHD��HD��HD�HD�AHD��HD�� D�  D�@ D�� D�� D�HD�@ D�~�D�� D�  D�@ D�� D�� D�HD�AHD�~�D�� D�  D�@ D�~�D���D���D�@ D���D��HD�  D�AHD�� D���D�  D�@ D�~�D���D�  D�@ D�~�D��qD�  D�AHD��HD�� D�  D�AHD��HD�� D�  D�AHD D�� D�  D�@ DÀ D��HD���D�=qD�~�D��HD��D�AHDŀ Dž�D�  D�AHDƁHD��HD�  D�@ Dǀ D�� D�HD�@ D�~�D��HD�  D�@ Dɀ D�� D�HD�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D̾�D���D�@ D̀ D;�D���D�=qD�}qD�� D�HD�>�Dπ D�� D���D�>�DЁHD�� D�  D�>�D�~�D�� D�  D�@ DҀ DҾ�D�  D�@ D�~�D�� D���D�@ DԁHD��HD�  D�@ DՀ D��HD�  D�@ Dր D�� D���D�AHDׁHD�� D�HD�AHD؀ Dؾ�D���D�@ Dـ D��HD�  D�>�Dڀ D�� D�  D�@ Dۀ D۾�D�  D�@ D�~�D�� D�  D�>�D݀ D��HD�  D�>�Dހ D��HD�  D�@ D߁HD�� D���D�@ D��HD��HD�  D�@ D�HD��HD�HD�@ D�HD��HD���D�>�D�~�D㾸D�HD�B�D� D侸D�  D�@ D�HD��HD�  D�@ D� D�� D�HD�@ D� D��HD�  D�AHD肏D�D�HD�@ D�~�D龸D�  D�B�D� D�qD��qD�@ D낏D�D�HD�@ D� D��HD��D�B�D�HD���D�HD�AHD�HD�D�HD�>�D�~�D�� D�  D�@ D�� D�� D�HD�@ D� D�� D�  D�@ D�~�D�� D�HD�@ D�~�D�� D�HD�@ D� D��HD�  D�>�D�� D�� D�HD�B�D��HD�� D�  D�AHD�� D�� D��D�B�D���D��HD�HD�@ D�~�D�� D�HD�>�D�~�D��HD��D�,�?\)?8Q�?u?�z�?�Q�?��?�@��@(�@(��@8Q�@G�@W
=@n{@}p�@���@���@�Q�@�  @�=q@�33@��H@�G�@˅@�@�p�@�ff@�\)@��HA�A�A
=qA�RA�
A��Ap�A"�\A&ffA+�A0��A5A:�HA?\)AC�
AH��AN�RATz�AY��A^{Ac33AhQ�An{As33AxQ�A|��A���A�(�A�ffA���A��\A��A��A��\A��A�
=A��A�z�A�
=A��A�z�A�
=A���A��
A�ffA�G�A�(�A��RA�G�A��
A�
=A��A���A�\)A�=qA���A�  A��HA�{Aأ�A�33A�{A���A�(�A�
=A陚A�(�A�RA�A���A�  A��\A��A��Bp�B�HBz�B�B\)B��B
{B\)B��B�\B  BG�B�\B  B��B33B��B�B\)B��B{B�B!�B"�\B#�
B%G�B&�\B((�B)B+
=B,z�B-B/
=B0��B2{B3�B4��B6ffB7�B8��B:=qB;�B=�B>ffB?�B@��BB=qBC�BE�BFffBG�BH��BJ{BK�BL��BN=qBO�BQ�BR�\BS�
BU�BV�\BX(�BY��B[
=B\Q�B]B_33B`��Bb=qBc�
Bep�Bf�RBhQ�Bi��Bk
=Bl��Bn{Bo�Bq�BrffBs�Btz�Bu��Bv�\Bw�Bxz�ByG�Bz{Bz�RB{
=B{�B|(�B|��B}��B~=qB~�RB33B�
B�=qB��\B��HB�33B���B��
B�(�B�z�B���B��B�p�B�B�(�B��\B��HB�G�B���B�  B�Q�B���B���B�G�B��B�{B�ffB���B�33B��B�  B�ffB���B��B�\)B�B�{B�z�B��HB�G�B��B�{B�ffB��RB�
=B�\)B�B�(�B��\B���B�\)B���B�  B�Q�B��RB���B�\)B�B�(�B��\B���B�G�B��B�  B�Q�B���B�
=B�\)B�B�=qB���B�
=B�p�B�B�(�B�z�B���B�33B���B�  B�ffB���B�33B���B�{B�z�B��HB�G�B���B�  B�ffB��RB��B�p�B��
B�=qB���B��B��B�  B�Q�B��RB��B��B��B�=qB���B���B�\)B�B�(�B��\B���B�\)B��
B�=qB��\B���B�G�B��B�  B�Q�B��RB��B�p�B��
B�=qB��RB��B��B��
B�=qB���B���B�\)B�B�{B�ffB��HB�33B���B�  B�ffB���B�33B���B�  B�ffB��RB��B�p�B��
B�=qB���B���B�\)B�B�(�B��\B�
=B�p�B��
B�=qB���B�
=B�\)B��
B�=qB��\B��HB�G�B��B�{B��\B���B�\)B�B�(�B\B�
=BÅB�  B�ffB���B�G�BŮB�(�BƏ\B�
=B�p�B��
B�Q�BȸRB�33BɮB�(�Bʏ\B�
=B˅B�  B�z�B�
=B�p�B��B�ffB���B�p�B��B�ffB���B�p�B��B�ffB��HB�p�B��B�ffB��HB�p�B��
B�ffB��HB�\)B��
B�ffB��HB�\)B��
B�ffB��HB�\)B��
B�ffB��HB�p�B��B�z�B���B߅B�{B��B�33B�B�=qB���B�\)B��B�z�B�
=B噚B�(�B��B�G�B�B�Q�B��HB�p�B�  B�\B��B�B�=qB�RB�\)B��B�z�B�
=BB�=qB���B�G�B��B�z�B���B�B�{B��RB�33B��
B�Q�B���B�p�B�  B��\B��B��B�=qB��HB�p�B�  B���B��B��B�Q�B��HB�p�C 
=C Q�C ��C �
C(�Cp�C�C  C=qC�\C��C{C\)C��C�HC�CffC��C�HC(�CffC�C��C=qCz�CC  CG�C�\C�
C�C\)C��C�C	33C	p�C	�RC	��C
=qC
z�C
C  C=qC�C��C  CG�C�CC
=CG�C�C��C
=CG�C�\C��C
=CQ�C�\C��C
=C=qCz�C�RC��C33Cp�C�C�C�C\)C��C�
C�C\)C��C�HC�C\)C��C�
C{CQ�C�CC  C=qCp�C�RC��C33Cp�C�RC�C(�CffC��C��C
=CG�Cz�CC��C33Cp�C�C�C�C\)C��C��C
=C=qC�C�RC��C=qCz�C�RC  C=qCp�C�C�C�C\)C��C�
C {C Q�C ��C �
C!
=C!G�C!�C!C"  C"=qC"�C"C#
=C#Q�C#��C#�
C$�C$\)C$��C$�
C%�C%ffC%�C&  C&=qC&�C&C'  C'=qC'�C'��C({C(ffC(�C(��C)33C)p�C)�RC*  C*G�C*��C*�HC+(�C+p�C+�RC+��C,=qC,�C,�
C-�C-p�C-C.
=C.\)C.��C.�HC/33C/z�C/�
C0(�C0z�C0C1
=C1Q�C1��C1��C2=qC2��C2�C333C3z�C3��C4{C4\)C4�RC5
=C5\)C5�C6  C6G�C6�\C6�HC733C7�C7�
C8(�C8z�C8C9
=C9ffC9�RC:{C:ffC:�RC;
=C;\)C;��C<  C<\)C<�RC=
=C=\)C=�C=��C>G�C>��C?  C?\)C?�RC@  C@Q�C@��CA  CA\)CA�RCB
=CB\)CB�CC
=CC\)CC�RCD{CDp�CDCE{CEffCE�RCF{CFz�CF�HCG(�CGz�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                ?�  @�\@B�\@}p�@��R@��R@�  A   A\)A   A+�A?\)A`��A���A��A��A�\)A��A�  A�Q�A��B   B  B�
B�B   B((�B0  B7�
B@  BH  BP  BX(�B`  Bg�
Bo�
Bxz�B�  B�  B��B��B��B�  B�  B��B�  B�  B�  B�  B�  B�{B�  B�  B�  B��B�  B�{B�  B�  B��B�  B�{B�  B��B��B�{B�  B��B��C   C
=C  C��C
=C
  C��C  C  C  C  C
=C
=C
=C
=C  C��C!��C#��C&  C(
=C*
=C,  C-��C0  C1��C3��C5��C8  C9��C;�C=�C?�CA��CD  CF
=CH  CJ  CL
=CN  CO��CR
=CT
=CV{CX{CZ
=C[��C^
=C`
=Cb
=Cd  Ce��Ch  Cj  Cl
=Cn
=Cp  Cr  Ct  Cv  Cx  Cz  C|  C~
=C�C�C�  C�  C�  C�  C���C���C���C���C�C�  C���C�  C�C�  C�  C�
=C�
=C���C���C�  C�
=C�C�C�  C�  C�
=C�
=C�C�
=C�C�  C�C�C�  C�
=C�  C���C�C�  C���C���C�  C�  C�  C���C���C���C�  C�  C���C�C�  C�  C���C���C�  C�  C�C�  C�  C�C���C�  C�  C�C�C�  C���C���C���C�C�C�C�  C�  C�C�C���C���C���C���C�  C�
=C�  C���C���C�  C�C�C�C�C�C�C�  C�C���C��C���C���C���C���C�  C�\C�C���C���C�  C�
=C�
=C�
=C�C�  C���C���C���C���C���C���C�  C�
=C�C�  C���C���C��C���D �D ��D  D� D  D� D  D}qD��D� D  D� D�D� D  D}qD�qD}qD�qD	z�D	��D
}qD�D� D�qD}qD  D�D  D}qD�D��D��D}qD�D� D�qDz�D  D��D��D}qD  D��DD�D�D� D�qD� D  Dz�D�qD� D�qD}qD�D��D�D��D�qD� D�D� D�qD }qD!  D!� D"  D"� D#  D#}qD$  D$��D%  D%� D&  D&� D'�D'�D(  D(� D)�D)� D)�qD*� D+  D+��D,  D,}qD,�qD-� D.  D.� D/�D/��D0�D0}qD1  D1� D2  D2� D3  D3� D4  D4}qD4�qD5� D6  D6� D6�qD7}qD7�qD8��D9�D9��D:�D:��D;  D;� D<�D<� D=  D=��D>  D>� D?  D?��D?�qD@� DA  DA��DB  DB� DC�DC��DD�DD� DE  DE}qDE�qDF� DF�qDGz�DG�qDH� DI�DI��DJ  DJ� DK�DK��DL�DL� DL�qDM}qDN�DN��DO  DO}qDO�qDP� DQ�DQ� DQ�qDR� DS  DS}qDT  DT� DT�qDU� DV  DV� DW  DW� DX  DX� DY  DY}qDY�qDZ}qDZ�qD[}qD[�qD\��D]�D]��D^�D^}qD^�qD_� D`  D`��Da�Da��Da�qDb}qDb�qDc� Dd  Dd� De  De��Df  Df� Dg  Dg}qDg�qDh��Di  Di}qDj  Dj��Dk�Dk��Dl  Dl}qDl�qDm}qDm�qDn}qDo�Do��Do�qDp� Dq�Dq� Dq�qDr}qDs  Ds� Dt  Dt��Dt�qDuz�Du��Dv}qDv�qDw� Dw�qDx� DyDy��Dz  Dz� Dz�qD{}qD|  D|� D}  D}� D~  D~� D~��D}qD�HD�AHD���D��HD���D�>�D�~�D���D�  D�@ D��HD���D�  D�AHD�� D��HD��D�AHD��HD�� D�  D�AHD��HD��HD���D�=qD�~�D��HD�HD�B�D�� D���D�HD�@ D�~�D��HD�HD�>�D�~�D�D��D�AHD��HD��HD�HD�>�D�� D�� D��qD�>�D��HD�D�HD�@ D�� D��HD�HD�AHD�� D�� D��qD�>�D�~�D���D���D�>�D��HD��HD���D�>�D�� D�� D�  D�AHD�� D���D�HD�@ D�� D���D��qD�@ D��HD���D���D�@ D�~�D��qD�  D�@ D�~�D�� D�  D�AHD��HD�� D���D�@ D��HD�� D�  D�B�D��HD�D�  D�=qD�� D�D��D�@ D�� D�D�HD�@ D�� D�� D���D�>�D�}qD�� D�HD�@ D�� D���D�  D�AHD�~�D���D�  D�B�D��HD�� D�HD�AHD�� D�� D�  D�>�D�~�D�� D�HD�@ D�� D�� D�HD�B�D��HD��HD�HD�AHD��HD��HD�  D�@ D��HD��HD��D�@ D�~�D�� D�HD�B�D��HD���D�  D�@ D�~�D��HD�  D�@ D�~�D���D�HD�B�D�� D��HD�HD�@ D��HD�D�  D�@ D�� D�� D�HD�@ D�� D�� D���D�@ D�� D�� D�  D�@ D��HD��HD�  D�AHD��HD�� D���D�>�D�~�D��HD�HD�@ D�~�D�� D�HD�AHD��HD��HD�  D�AHD��HD��HD�HD�AHD��HD�� D�  D�@ D�� D�� D�HD�@ D�~�D�� D�  D�@ D�� D�� D�HD�AHD�~�D�� D�  D�@ D�~�D���D���D�@ D���D��HD�  D�AHD�� D���D�  D�@ D�~�D���D�  D�@ D�~�D��qD�  D�AHD��HD�� D�  D�AHD��HD�� D�  D�AHD D�� D�  D�@ DÀ D��HD���D�=qD�~�D��HD��D�AHDŀ Dž�D�  D�AHDƁHD��HD�  D�@ Dǀ D�� D�HD�@ D�~�D��HD�  D�@ Dɀ D�� D�HD�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D̾�D���D�@ D̀ D;�D���D�=qD�}qD�� D�HD�>�Dπ D�� D���D�>�DЁHD�� D�  D�>�D�~�D�� D�  D�@ DҀ DҾ�D�  D�@ D�~�D�� D���D�@ DԁHD��HD�  D�@ DՀ D��HD�  D�@ Dր D�� D���D�AHDׁHD�� D�HD�AHD؀ Dؾ�D���D�@ Dـ D��HD�  D�>�Dڀ D�� D�  D�@ Dۀ D۾�D�  D�@ D�~�D�� D�  D�>�D݀ D��HD�  D�>�Dހ D��HD�  D�@ D߁HD�� D���D�@ D��HD��HD�  D�@ D�HD��HD�HD�@ D�HD��HD���D�>�D�~�D㾸D�HD�B�D� D侸D�  D�@ D�HD��HD�  D�@ D� D�� D�HD�@ D� D��HD�  D�AHD肏D�D�HD�@ D�~�D龸D�  D�B�D� D�qD��qD�@ D낏D�D�HD�@ D� D��HD��D�B�D�HD���D�HD�AHD�HD�D�HD�>�D�~�D�� D�  D�@ D�� D�� D�HD�@ D� D�� D�  D�@ D�~�D�� D�HD�@ D�~�D�� D�HD�@ D� D��HD�  D�>�D�� D�� D�HD�B�D��HD�� D�  D�AHD�� D�� D��D�B�D���D��HD�HD�@ D�~�D�� D�HD�>�D�~�D��HD��D�,�?\)?8Q�?u?�z�?�Q�?��?�@��@(�@(��@8Q�@G�@W
=@n{@}p�@���@���@�Q�@�  @�=q@�33@��H@�G�@˅@�@�p�@�ff@�\)@��HA�A�A
=qA�RA�
A��Ap�A"�\A&ffA+�A0��A5A:�HA?\)AC�
AH��AN�RATz�AY��A^{Ac33AhQ�An{As33AxQ�A|��A���A�(�A�ffA���A��\A��A��A��\A��A�
=A��A�z�A�
=A��A�z�A�
=A���A��
A�ffA�G�A�(�A��RA�G�A��
A�
=A��A���A�\)A�=qA���A�  A��HA�{Aأ�A�33A�{A���A�(�A�
=A陚A�(�A�RA�A���A�  A��\A��A��Bp�B�HBz�B�B\)B��B
{B\)B��B�\B  BG�B�\B  B��B33B��B�B\)B��B{B�B!�B"�\B#�
B%G�B&�\B((�B)B+
=B,z�B-B/
=B0��B2{B3�B4��B6ffB7�B8��B:=qB;�B=�B>ffB?�B@��BB=qBC�BE�BFffBG�BH��BJ{BK�BL��BN=qBO�BQ�BR�\BS�
BU�BV�\BX(�BY��B[
=B\Q�B]B_33B`��Bb=qBc�
Bep�Bf�RBhQ�Bi��Bk
=Bl��Bn{Bo�Bq�BrffBs�Btz�Bu��Bv�\Bw�Bxz�ByG�Bz{Bz�RB{
=B{�B|(�B|��B}��B~=qB~�RB33B�
B�=qB��\B��HB�33B���B��
B�(�B�z�B���B��B�p�B�B�(�B��\B��HB�G�B���B�  B�Q�B���B���B�G�B��B�{B�ffB���B�33B��B�  B�ffB���B��B�\)B�B�{B�z�B��HB�G�B��B�{B�ffB��RB�
=B�\)B�B�(�B��\B���B�\)B���B�  B�Q�B��RB���B�\)B�B�(�B��\B���B�G�B��B�  B�Q�B���B�
=B�\)B�B�=qB���B�
=B�p�B�B�(�B�z�B���B�33B���B�  B�ffB���B�33B���B�{B�z�B��HB�G�B���B�  B�ffB��RB��B�p�B��
B�=qB���B��B��B�  B�Q�B��RB��B��B��B�=qB���B���B�\)B�B�(�B��\B���B�\)B��
B�=qB��\B���B�G�B��B�  B�Q�B��RB��B�p�B��
B�=qB��RB��B��B��
B�=qB���B���B�\)B�B�{B�ffB��HB�33B���B�  B�ffB���B�33B���B�  B�ffB��RB��B�p�B��
B�=qB���B���B�\)B�B�(�B��\B�
=B�p�B��
B�=qB���B�
=B�\)B��
B�=qB��\B��HB�G�B��B�{B��\B���B�\)B�B�(�B\B�
=BÅB�  B�ffB���B�G�BŮB�(�BƏ\B�
=B�p�B��
B�Q�BȸRB�33BɮB�(�Bʏ\B�
=B˅B�  B�z�B�
=B�p�B��B�ffB���B�p�B��B�ffB���B�p�B��B�ffB��HB�p�B��B�ffB��HB�p�B��
B�ffB��HB�\)B��
B�ffB��HB�\)B��
B�ffB��HB�\)B��
B�ffB��HB�p�B��B�z�B���B߅B�{B��B�33B�B�=qB���B�\)B��B�z�B�
=B噚B�(�B��B�G�B�B�Q�B��HB�p�B�  B�\B��B�B�=qB�RB�\)B��B�z�B�
=BB�=qB���B�G�B��B�z�B���B�B�{B��RB�33B��
B�Q�B���B�p�B�  B��\B��B��B�=qB��HB�p�B�  B���B��B��B�Q�B��HB�p�C 
=C Q�C ��C �
C(�Cp�C�C  C=qC�\C��C{C\)C��C�HC�CffC��C�HC(�CffC�C��C=qCz�CC  CG�C�\C�
C�C\)C��C�C	33C	p�C	�RC	��C
=qC
z�C
C  C=qC�C��C  CG�C�CC
=CG�C�C��C
=CG�C�\C��C
=CQ�C�\C��C
=C=qCz�C�RC��C33Cp�C�C�C�C\)C��C�
C�C\)C��C�HC�C\)C��C�
C{CQ�C�CC  C=qCp�C�RC��C33Cp�C�RC�C(�CffC��C��C
=CG�Cz�CC��C33Cp�C�C�C�C\)C��C��C
=C=qC�C�RC��C=qCz�C�RC  C=qCp�C�C�C�C\)C��C�
C {C Q�C ��C �
C!
=C!G�C!�C!C"  C"=qC"�C"C#
=C#Q�C#��C#�
C$�C$\)C$��C$�
C%�C%ffC%�C&  C&=qC&�C&C'  C'=qC'�C'��C({C(ffC(�C(��C)33C)p�C)�RC*  C*G�C*��C*�HC+(�C+p�C+�RC+��C,=qC,�C,�
C-�C-p�C-C.
=C.\)C.��C.�HC/33C/z�C/�
C0(�C0z�C0C1
=C1Q�C1��C1��C2=qC2��C2�C333C3z�C3��C4{C4\)C4�RC5
=C5\)C5�C6  C6G�C6�\C6�HC733C7�C7�
C8(�C8z�C8C9
=C9ffC9�RC:{C:ffC:�RC;
=C;\)C;��C<  C<\)C<�RC=
=C=\)C=�C=��C>G�C>��C?  C?\)C?�RC@  C@Q�C@��CA  CA\)CA�RCB
=CB\)CB�CC
=CC\)CC�RCD{CDp�CDCE{CEffCE�RCF{CFz�CF�HCG(�CGz�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Q�A�VA�ZA�bNA�dZA�bNA�bNA�\)A�ZA�ZA�\)A�ZA�bNA�dZA�l�AҁA҃A�n�A�z�A�~�A�^5A�A�A�G�A��A�1A�  A��A���A�bNA�A��TA��
Aď\A�l�A���A�=qA�5?A�-A��\A���A��A�E�A��A��RA��A���A�~�A��A���A�l�A��A�G�A���A���A�E�A��;A���A���A�A���A�ffA�t�A��A�ffA���A��!A�t�A�$�A�n�A�S�A�%A���A�ĜA~��Az1'Av1Aq�#Ap�Al$�Af��Ac?}AaG�A`JA\�HAW�FAV��AS�AQ�mAPȴAN�AM�AM��AL�AL5?AJ(�AIS�AH�\AG��AF(�AEoAB�`ABA�AA
=A@�A?t�A?�A>�A>�/A>A�A=%A;A;7LA:ĜA:r�A9�A8-A7�
A7|�A7;dA6��A6�uA6jA6bNA6bA5�PA4�A4-A3hsA2�yA2��A21A1�A0E�A.ffA-\)A,�+A,=qA+�A+%A*ZA);dA(^5A'�A'x�A'%A&�A&��A&ZA&1A%��A%�wA&=qA&�A%dZA%A$�HA$�jA$�DA${A#��A#XA#?}A"��A"��A"��A"��A"I�A!A!�7A!\)A!;dA!"�A!�A!A ��A E�A  �A�
A��AG�AȴAr�A��A\)A��AE�AbA�A�-A\)AA��A5?AK�A�A��A��AbA\)A"�A��A^5A|�A+AjA��A�wA&�A�\AJA  A�
A�wA��A&�A�`A�!AffA�AƨA�hAG�A�HAZAA�AAO�A33A�AZA9XA�A��A��AƨA�hA
=A
�yA
�A
�jA
��A
n�A
^5A
E�A
JA	�FA	|�A��AȴA��AI�A�mA�hA
=A^5A��AC�Ar�AAAl�AVA��A��AZA�wAO�A �\A (�@��
@�\)@�@���@�@��h@�?}@� �@�+@��!@�@��7@���@��F@���@�ff@��#@���@�@��/@�@��H@��@��@�I�@�ƨ@�\)@��@��@�V@��T@�O�@�z�@��
@�;d@�-@�V@�ƨ@�t�@�K�@�C�@�"�@��y@�+@���@�7@�X@��@���@޸R@�hs@���@��@�z�@�Z@�Z@�Z@�Z@�Z@�Q�@�C�@֏\@Ցh@�z�@�\)@�$�@��@щ7@�G�@��@�I�@��
@Ͼw@�t�@�+@�@���@Ͳ-@�G�@��@̼j@̣�@̋D@�r�@���@�V@�p�@ȼj@�j@�A�@�1'@��m@ǥ�@�t�@�+@��H@Ə\@�^5@��@ř�@�hs@�&�@��/@ēu@�Q�@�  @���@���@�/@��@� �@�  @���@��m@��;@��w@�l�@��@���@���@�I�@��@�t�@�"�@���@�&�@��j@�j@�  @�l�@��@�M�@��@���@���@�`B@��@���@�j@�A�@�Q�@��m@���@�;d@��@���@�=q@���@��^@�`B@��`@�bN@� �@���@�t�@��H@��!@���@�ff@�$�@��@��-@���@��j@��9@�bN@���@�\)@��!@�ff@��@���@��@�Ĝ@���@�j@��;@�33@���@��#@�&�@��D@�Z@�  @���@�;d@�
=@��\@�5?@��@���@��-@���@��7@�7L@��@� �@���@���@�l�@��@��\@�@��^@��@�?}@�V@���@��D@�bN@�Q�@�(�@��m@���@�K�@�o@��@�ff@�-@��@��h@���@��j@��@�z�@�Q�@��@��m@��F@�S�@�33@�"�@�
=@���@�n�@�V@�M�@�5?@�@���@��7@�%@���@���@��@�bN@�(�@���@�K�@�
=@��!@�n�@�E�@�$�@��T@�p�@�V@���@��/@��u@�z�@�1'@���@�dZ@���@�ȴ@�v�@�ff@�^5@�V@�=q@��@��T@��h@�hs@�O�@�/@�%@���@���@�r�@�1@��@�|�@�\)@�K�@�
=@���@�n�@�-@�{@��#@�x�@�7L@��@��@���@��F@��@�;d@�"�@�"�@�
=@���@��+@�M�@�-@��@�J@��@�x�@�7L@���@���@���@�Z@� �@�1@��m@�ƨ@�t�@�"�@��y@���@�ff@�=q@���@���@���@�&�@���@���@�A�@�b@��@|�@\)@+@~��@~ȴ@~5?@}�@}��@}?}@|�@|�j@|�D@|9X@{�F@z��@z=q@y��@x��@xbN@x  @w�w@w��@w�P@wK�@v��@vv�@u�T@uO�@t�@t�j@t�@t�D@t1@s�F@st�@r�!@r=q@r�@q�@q7L@p �@o�@o�@n�@nȴ@n5?@m�h@mV@l�j@lZ@k��@kC�@j��@i�@i7L@hbN@g�w@g;d@f��@f�R@e�T@e�@ep�@d��@dI�@c�m@c�@c�@cS�@c33@cC�@b��@bJ@a�^@aX@aG�@a�@`��@`�u@`r�@`  @_�@_K�@_�@^�+@^{@]/@\��@\Z@\9X@\(�@[��@[�F@Z�H@Z-@Yhs@X�`@X�u@Xr�@XA�@Xb@W�@W;d@Vȴ@V�+@Vv�@Vv�@Vff@Vff@V$�@U@UV@S��@St�@SdZ@SS�@S"�@S@R�@R��@Q�7@P�9@PbN@P  @O�@O�@O�;@O�;@O�@O|�@OK�@O
=@N�+@N$�@M�T@M�h@M/@L�/@L�j@L�D@Lj@LI�@L9X@L(�@L�@L1@K�
@K�F@K��@KC�@J�@IG�@H�9@Hr�@Hr�@HbN@HQ�@HA�@Hb@G�@G�P@F�@FV@E��@E`B@E?}@D�@Dj@D�@C�F@CdZ@C33@C"�@B�H@Bn�@BJ@A��@A�@@��@@��@@Q�@?�w@?|�@?+@>�@>��@>E�@=�@=��@=p�@=?}@<�@<��@<�@<�D@<�D@<I�@;��@;"�@;@:�H@:�!@:n�@:J@9��@9�@8Ĝ@8�@8A�@8b@7�w@7l�@7\)@7K�@6ȴ@6V@6E�@6$�@5��@5O�@5�@5�@5V@4��@4�@4�/@4��@4�@4Z@4I�@3�m@3�F@3��@3��@3�@3t�@333@333@3@2��@2^5@1�@1x�@1&�@0�`@0��@0bN@0A�@/�;@/;d@/�@.�@.�R@.�+@.V@.$�@-�h@-�@-`B@-O�@-?}@-�@-�@,��@,j@+�m@+t�@+33@+@*��@*��@*�\@*J@)��@)G�@)�@(Ĝ@(��@(�u@'�@'+@&�@&�+@&V@&{@%��@%`B@%/@$��@$�D@#��@#��@#S�@#@"��@"�\@"^5@"=q@"J@!hs@!�@!%@ ��@ �u@ r�@  �@�@�P@l�@+@�@�R@��@�+@ff@E�@�-@�@?}@�@V@�j@j@1@�F@��@t�@S�@C�@33@o@�H@�!@n�@�@�^@G�@7L@&�@&�@��@Ĝ@�u@bN@  @�;@��@�w@�P@K�@�y@��@{@�h@/@��@��@�j@�j@�@z�@Z@9X@��@�m@�
@�F@t�@"�@�@��@��@��@�!@�!@��@-@��@��@�@�`@�9@Q�@�@�w@�@��@l�@�y@�+@E�@��@@@@@�-@�@p�@O�@�@V@V@��@�@�@�A�E�A�O�A�XA�Q�A�O�A�O�A�VA�VA�\)A�S�A�VA�ZA�ffA�dZA�ffA�bNA�`BA�bNA�ffA�ffA�bNA�`BA�bNA�ffA�dZA�`BA�\)A�bNA�^5A�\)A�XA�ZA�ZA�\)A�ZA�XA�XA�^5A�^5A�bNA�\)A�ZA�VA�S�A�ZA�`BA�dZA�`BA�^5A�`BA�dZA�ffA�bNA�bNA�bNA�ffA�hsA�dZA�bNA�dZA�dZA�hsA�n�A�r�A�|�A҃A҉7A҉7AҁA�z�A�v�A�v�A�z�AҁA҉7AҋDA҉7AҁA�jA�ffA�jA�hsA�p�A�v�A�z�A�|�A�~�A�~�A�~�AҁAҁA҃A҃A�v�A�n�A�ffA�`BA�ZA�XA�O�A�E�A�=qA�9XA�;dA�E�A�M�A�O�A�M�A�E�A�=qA�7LA�-A�$�A��A�bA�VA�JA�VA�JA�
=A�A���A���A�  A�A�A�  A���A���A���A���A��A��A��TA��/A��A���A�ȴAѲ-Aѝ�Aч+A�jA�VA�5?A�VA���A�dZA�A���Aω7A��AάA͗�A̴9A�ZA�ĜA�=qA�ZA��Aǝ�A��A�Ař�A�|�A�t�A�ZA���A�n�A��`A���A�r�A���A�-A�A���A�+A�ĜA��A�K�A�K�A�VA��A�ĜA�;dA�{A��A���A�t�A�C�A��A�A��mA���A��9A���A��+A�v�A�hsA�XA�K�A�C�A�A�A�C�A�?}A�/A��A��/A�VA��FA�XA��mA�|�A���A��+A�I�A��A�ȴA�bNA�ȴA�`BA�/A��A��A���A��wA���A�x�A�K�A�{A���A��7A�ZA�5?A�{A��A��/A���A��wA��A���A��uA��A�r�A�dZA�ZA�Q�A�I�A�?}A�7LA�-A��A�JA�%A���A��`A�ȴA��FA��A���A���A�;dA�  A���A���A�G�A�{A���A���A�ĜA��!A���A�n�A�S�A�9XA�VA��yA�ȴA��A���A�z�A�ffA�XA�1'A���A���A��PA�-A��A���A��FA���A�ffA�5?A��A�1A���A��HA���A��FA���A��A�\)A�"�A��yA���A��jA��A���A��A���A���A��uA�z�A�S�A���A���A�^5A��A��
A���A�~�A�K�A�"�A�JA�A���A��A��TA��
A�ĜA���A�|�A�$�A��`A���A���A�A��jA��jA��^A��RA��!A��A���A���A���A��hA��7A��+A�|�A�p�A�bNA�K�A�/A���A��mA��HA��TA��mA��A��A��A��mA��;A��wA���A�x�A�XA��A���A���A�`BA�JA���A��/A�A��9A��-A���A���A���A�~�A�?}A�A�A�t�A�M�A�-A�bA�  A��A��
A��jA���A�`BA�5?A� �A�JA���A��#A��RA���A�r�A�M�A�"�A���A��A��FA��\A�^5A�&�A��yA���A�|�A�O�A�%A���A���A�t�A�/A���A���A��+A��7A���A���A���A��-A��FA��RA��!A�x�A�C�A��A�VA���A��#A��wA��A�~�A�M�A�33A���A���A�O�A�A��A�+A��HA���A�VA�9XA��A�ȴA�5?A���A�$�A���A�=qA�VA���A��A��#A��-A�M�A��A��A��;A�A���A�S�A��A��/A��^A���A�v�A�=qA�1A��HA��A��A�ffA�Q�A��yA��\A�(�A��A���A�x�A�\)A�A�A��A�  A��
A��A�hsA�I�A�5?A��A��TA���A��A�XA�A���A���A�^5A�1A�t�A�
=A��A���A�r�A�/A�1A��TA��FA���A�S�A���A���A��+A���A��/A���A��A~ �A}&�A|�A{�
A{�FA{x�Az�\Ay�
Ay��Ay|�Ay\)Ax��AxVAwAw33Av(�At��AsƨAs"�Ar��Ar1'Ar1Aq�mAq��Aq�Aq�PAql�Aq;dAqAp�jAp~�Ap�Ao��Ao`BAoO�Ao"�AnZAm�hAmoAl1'Ak��Akx�AjbNAh��AgƨAgoAf�HAf�\AfffAf9XAe�Ae�^Ael�Ad��Ac�;Ac
=AbȴAb�DAbr�AbQ�Ab-Aa�Aa�^Aa|�AaS�Aa"�A`�yA`�/A`�jA`�A`�\A`r�A`M�A`5?A`A_�wA_�7A_S�A_oA^��A^jA^{A]�FA\��A[XAZbAY�FAYl�AX�HAW��AWVAV�AV�yAV�yAV�/AV��AVȴAVĜAV�9AV��AV�+AVv�AVZAV�AU��AU7LATffAS;dAR�RAR�+ARr�ARM�ARE�AR-AR1AQ�AQ�-AQp�AQS�AQ+AP�APȴAP�jAP�/AP��AP��AO��AO?}AOoAO&�AOoAN�/ANv�ANVAN(�ANbAM�AM�
AM��AM��AM�;AN1AN5?AM��ANAM��ALȴAL��AL��AM�AM%AL�`AL�AL��AL�AL�\ALr�AL=qAL9XAL-AK�-AK�AJ�uAJE�AJ  AI�
AI��AI�AI�AI|�AIl�AIK�AI&�AI�AIVAIAH�yAH~�AHA�AH$�AHJAG��AG�
AG�-AG�hAGx�AGdZAG33AFr�AFbAE�AE�;AE�
AE�
AE��AE�FAE;dAD�uAC�
AC�PAC+ACABĜAB��AB�\ABz�ABI�ABVABVABQ�AA��AA��AAAAA@�yA@�A@��A@��A@ĜA@�RA@��A@ffA?��A?��A?��A?dZA?dZA?`BA?\)A??}A?"�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                A�Q�A�VA�ZA�bNA�dZA�bNA�bNA�\)A�ZA�ZA�\)A�ZA�bNA�dZA�l�AҁA҃A�n�A�z�A�~�A�^5A�A�A�G�A��A�1A�  A��A���A�bNA�A��TA��
Aď\A�l�A���A�=qA�5?A�-A��\A���A��A�E�A��A��RA��A���A�~�A��A���A�l�A��A�G�A���A���A�E�A��;A���A���A�A���A�ffA�t�A��A�ffA���A��!A�t�A�$�A�n�A�S�A�%A���A�ĜA~��Az1'Av1Aq�#Ap�Al$�Af��Ac?}AaG�A`JA\�HAW�FAV��AS�AQ�mAPȴAN�AM�AM��AL�AL5?AJ(�AIS�AH�\AG��AF(�AEoAB�`ABA�AA
=A@�A?t�A?�A>�A>�/A>A�A=%A;A;7LA:ĜA:r�A9�A8-A7�
A7|�A7;dA6��A6�uA6jA6bNA6bA5�PA4�A4-A3hsA2�yA2��A21A1�A0E�A.ffA-\)A,�+A,=qA+�A+%A*ZA);dA(^5A'�A'x�A'%A&�A&��A&ZA&1A%��A%�wA&=qA&�A%dZA%A$�HA$�jA$�DA${A#��A#XA#?}A"��A"��A"��A"��A"I�A!A!�7A!\)A!;dA!"�A!�A!A ��A E�A  �A�
A��AG�AȴAr�A��A\)A��AE�AbA�A�-A\)AA��A5?AK�A�A��A��AbA\)A"�A��A^5A|�A+AjA��A�wA&�A�\AJA  A�
A�wA��A&�A�`A�!AffA�AƨA�hAG�A�HAZAA�AAO�A33A�AZA9XA�A��A��AƨA�hA
=A
�yA
�A
�jA
��A
n�A
^5A
E�A
JA	�FA	|�A��AȴA��AI�A�mA�hA
=A^5A��AC�Ar�AAAl�AVA��A��AZA�wAO�A �\A (�@��
@�\)@�@���@�@��h@�?}@� �@�+@��!@�@��7@���@��F@���@�ff@��#@���@�@��/@�@��H@��@��@�I�@�ƨ@�\)@��@��@�V@��T@�O�@�z�@��
@�;d@�-@�V@�ƨ@�t�@�K�@�C�@�"�@��y@�+@���@�7@�X@��@���@޸R@�hs@���@��@�z�@�Z@�Z@�Z@�Z@�Z@�Q�@�C�@֏\@Ցh@�z�@�\)@�$�@��@щ7@�G�@��@�I�@��
@Ͼw@�t�@�+@�@���@Ͳ-@�G�@��@̼j@̣�@̋D@�r�@���@�V@�p�@ȼj@�j@�A�@�1'@��m@ǥ�@�t�@�+@��H@Ə\@�^5@��@ř�@�hs@�&�@��/@ēu@�Q�@�  @���@���@�/@��@� �@�  @���@��m@��;@��w@�l�@��@���@���@�I�@��@�t�@�"�@���@�&�@��j@�j@�  @�l�@��@�M�@��@���@���@�`B@��@���@�j@�A�@�Q�@��m@���@�;d@��@���@�=q@���@��^@�`B@��`@�bN@� �@���@�t�@��H@��!@���@�ff@�$�@��@��-@���@��j@��9@�bN@���@�\)@��!@�ff@��@���@��@�Ĝ@���@�j@��;@�33@���@��#@�&�@��D@�Z@�  @���@�;d@�
=@��\@�5?@��@���@��-@���@��7@�7L@��@� �@���@���@�l�@��@��\@�@��^@��@�?}@�V@���@��D@�bN@�Q�@�(�@��m@���@�K�@�o@��@�ff@�-@��@��h@���@��j@��@�z�@�Q�@��@��m@��F@�S�@�33@�"�@�
=@���@�n�@�V@�M�@�5?@�@���@��7@�%@���@���@��@�bN@�(�@���@�K�@�
=@��!@�n�@�E�@�$�@��T@�p�@�V@���@��/@��u@�z�@�1'@���@�dZ@���@�ȴ@�v�@�ff@�^5@�V@�=q@��@��T@��h@�hs@�O�@�/@�%@���@���@�r�@�1@��@�|�@�\)@�K�@�
=@���@�n�@�-@�{@��#@�x�@�7L@��@��@���@��F@��@�;d@�"�@�"�@�
=@���@��+@�M�@�-@��@�J@��@�x�@�7L@���@���@���@�Z@� �@�1@��m@�ƨ@�t�@�"�@��y@���@�ff@�=q@���@���@���@�&�@���@���@�A�@�b@��@|�@\)@+@~��@~ȴ@~5?@}�@}��@}?}@|�@|�j@|�D@|9X@{�F@z��@z=q@y��@x��@xbN@x  @w�w@w��@w�P@wK�@v��@vv�@u�T@uO�@t�@t�j@t�@t�D@t1@s�F@st�@r�!@r=q@r�@q�@q7L@p �@o�@o�@n�@nȴ@n5?@m�h@mV@l�j@lZ@k��@kC�@j��@i�@i7L@hbN@g�w@g;d@f��@f�R@e�T@e�@ep�@d��@dI�@c�m@c�@c�@cS�@c33@cC�@b��@bJ@a�^@aX@aG�@a�@`��@`�u@`r�@`  @_�@_K�@_�@^�+@^{@]/@\��@\Z@\9X@\(�@[��@[�F@Z�H@Z-@Yhs@X�`@X�u@Xr�@XA�@Xb@W�@W;d@Vȴ@V�+@Vv�@Vv�@Vff@Vff@V$�@U@UV@S��@St�@SdZ@SS�@S"�@S@R�@R��@Q�7@P�9@PbN@P  @O�@O�@O�;@O�;@O�@O|�@OK�@O
=@N�+@N$�@M�T@M�h@M/@L�/@L�j@L�D@Lj@LI�@L9X@L(�@L�@L1@K�
@K�F@K��@KC�@J�@IG�@H�9@Hr�@Hr�@HbN@HQ�@HA�@Hb@G�@G�P@F�@FV@E��@E`B@E?}@D�@Dj@D�@C�F@CdZ@C33@C"�@B�H@Bn�@BJ@A��@A�@@��@@��@@Q�@?�w@?|�@?+@>�@>��@>E�@=�@=��@=p�@=?}@<�@<��@<�@<�D@<�D@<I�@;��@;"�@;@:�H@:�!@:n�@:J@9��@9�@8Ĝ@8�@8A�@8b@7�w@7l�@7\)@7K�@6ȴ@6V@6E�@6$�@5��@5O�@5�@5�@5V@4��@4�@4�/@4��@4�@4Z@4I�@3�m@3�F@3��@3��@3�@3t�@333@333@3@2��@2^5@1�@1x�@1&�@0�`@0��@0bN@0A�@/�;@/;d@/�@.�@.�R@.�+@.V@.$�@-�h@-�@-`B@-O�@-?}@-�@-�@,��@,j@+�m@+t�@+33@+@*��@*��@*�\@*J@)��@)G�@)�@(Ĝ@(��@(�u@'�@'+@&�@&�+@&V@&{@%��@%`B@%/@$��@$�D@#��@#��@#S�@#@"��@"�\@"^5@"=q@"J@!hs@!�@!%@ ��@ �u@ r�@  �@�@�P@l�@+@�@�R@��@�+@ff@E�@�-@�@?}@�@V@�j@j@1@�F@��@t�@S�@C�@33@o@�H@�!@n�@�@�^@G�@7L@&�@&�@��@Ĝ@�u@bN@  @�;@��@�w@�P@K�@�y@��@{@�h@/@��@��@�j@�j@�@z�@Z@9X@��@�m@�
@�F@t�@"�@�@��@��@��@�!@�!@��@-@��@��@�@�`@�9@Q�@�@�w@�@��@l�@�y@�+@E�@��@@@@@�-@�@p�@O�@�@V@V@��@�@�@�A�E�A�O�A�XA�Q�A�O�A�O�A�VA�VA�\)A�S�A�VA�ZA�ffA�dZA�ffA�bNA�`BA�bNA�ffA�ffA�bNA�`BA�bNA�ffA�dZA�`BA�\)A�bNA�^5A�\)A�XA�ZA�ZA�\)A�ZA�XA�XA�^5A�^5A�bNA�\)A�ZA�VA�S�A�ZA�`BA�dZA�`BA�^5A�`BA�dZA�ffA�bNA�bNA�bNA�ffA�hsA�dZA�bNA�dZA�dZA�hsA�n�A�r�A�|�A҃A҉7A҉7AҁA�z�A�v�A�v�A�z�AҁA҉7AҋDA҉7AҁA�jA�ffA�jA�hsA�p�A�v�A�z�A�|�A�~�A�~�A�~�AҁAҁA҃A҃A�v�A�n�A�ffA�`BA�ZA�XA�O�A�E�A�=qA�9XA�;dA�E�A�M�A�O�A�M�A�E�A�=qA�7LA�-A�$�A��A�bA�VA�JA�VA�JA�
=A�A���A���A�  A�A�A�  A���A���A���A���A��A��A��TA��/A��A���A�ȴAѲ-Aѝ�Aч+A�jA�VA�5?A�VA���A�dZA�A���Aω7A��AάA͗�A̴9A�ZA�ĜA�=qA�ZA��Aǝ�A��A�Ař�A�|�A�t�A�ZA���A�n�A��`A���A�r�A���A�-A�A���A�+A�ĜA��A�K�A�K�A�VA��A�ĜA�;dA�{A��A���A�t�A�C�A��A�A��mA���A��9A���A��+A�v�A�hsA�XA�K�A�C�A�A�A�C�A�?}A�/A��A��/A�VA��FA�XA��mA�|�A���A��+A�I�A��A�ȴA�bNA�ȴA�`BA�/A��A��A���A��wA���A�x�A�K�A�{A���A��7A�ZA�5?A�{A��A��/A���A��wA��A���A��uA��A�r�A�dZA�ZA�Q�A�I�A�?}A�7LA�-A��A�JA�%A���A��`A�ȴA��FA��A���A���A�;dA�  A���A���A�G�A�{A���A���A�ĜA��!A���A�n�A�S�A�9XA�VA��yA�ȴA��A���A�z�A�ffA�XA�1'A���A���A��PA�-A��A���A��FA���A�ffA�5?A��A�1A���A��HA���A��FA���A��A�\)A�"�A��yA���A��jA��A���A��A���A���A��uA�z�A�S�A���A���A�^5A��A��
A���A�~�A�K�A�"�A�JA�A���A��A��TA��
A�ĜA���A�|�A�$�A��`A���A���A�A��jA��jA��^A��RA��!A��A���A���A���A��hA��7A��+A�|�A�p�A�bNA�K�A�/A���A��mA��HA��TA��mA��A��A��A��mA��;A��wA���A�x�A�XA��A���A���A�`BA�JA���A��/A�A��9A��-A���A���A���A�~�A�?}A�A�A�t�A�M�A�-A�bA�  A��A��
A��jA���A�`BA�5?A� �A�JA���A��#A��RA���A�r�A�M�A�"�A���A��A��FA��\A�^5A�&�A��yA���A�|�A�O�A�%A���A���A�t�A�/A���A���A��+A��7A���A���A���A��-A��FA��RA��!A�x�A�C�A��A�VA���A��#A��wA��A�~�A�M�A�33A���A���A�O�A�A��A�+A��HA���A�VA�9XA��A�ȴA�5?A���A�$�A���A�=qA�VA���A��A��#A��-A�M�A��A��A��;A�A���A�S�A��A��/A��^A���A�v�A�=qA�1A��HA��A��A�ffA�Q�A��yA��\A�(�A��A���A�x�A�\)A�A�A��A�  A��
A��A�hsA�I�A�5?A��A��TA���A��A�XA�A���A���A�^5A�1A�t�A�
=A��A���A�r�A�/A�1A��TA��FA���A�S�A���A���A��+A���A��/A���A��A~ �A}&�A|�A{�
A{�FA{x�Az�\Ay�
Ay��Ay|�Ay\)Ax��AxVAwAw33Av(�At��AsƨAs"�Ar��Ar1'Ar1Aq�mAq��Aq�Aq�PAql�Aq;dAqAp�jAp~�Ap�Ao��Ao`BAoO�Ao"�AnZAm�hAmoAl1'Ak��Akx�AjbNAh��AgƨAgoAf�HAf�\AfffAf9XAe�Ae�^Ael�Ad��Ac�;Ac
=AbȴAb�DAbr�AbQ�Ab-Aa�Aa�^Aa|�AaS�Aa"�A`�yA`�/A`�jA`�A`�\A`r�A`M�A`5?A`A_�wA_�7A_S�A_oA^��A^jA^{A]�FA\��A[XAZbAY�FAYl�AX�HAW��AWVAV�AV�yAV�yAV�/AV��AVȴAVĜAV�9AV��AV�+AVv�AVZAV�AU��AU7LATffAS;dAR�RAR�+ARr�ARM�ARE�AR-AR1AQ�AQ�-AQp�AQS�AQ+AP�APȴAP�jAP�/AP��AP��AO��AO?}AOoAO&�AOoAN�/ANv�ANVAN(�ANbAM�AM�
AM��AM��AM�;AN1AN5?AM��ANAM��ALȴAL��AL��AM�AM%AL�`AL�AL��AL�AL�\ALr�AL=qAL9XAL-AK�-AK�AJ�uAJE�AJ  AI�
AI��AI�AI�AI|�AIl�AIK�AI&�AI�AIVAIAH�yAH~�AHA�AH$�AHJAG��AG�
AG�-AG�hAGx�AGdZAG33AFr�AFbAE�AE�;AE�
AE�
AE��AE�FAE;dAD�uAC�
AC�PAC+ACABĜAB��AB�\ABz�ABI�ABVABVABQ�AA��AA��AAAAA@�yA@�A@��A@��A@ĜA@�RA@��A@ffA?��A?��A?��A?dZA?dZA?`BA?\)A??}A?"�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B��B� B�5B�5B�5B� B��B�B�iB��B�B�B��B��B	y	B	ϫB
C-B
��B
�7B
��B
��B
��B
�:B
�bB
�hB
��B
��B
��B
�B
��B
�B
�JB
Y�B
W?B
�qB
�<B
��B
�B
�KB
��B�B�B �B)_B/OB.B;0B9�B=<B4�B6zBb�B`�B^�BY�BOBBFB3�BVB7B.IBIBuB
�B
ΥB
��B
�B
��B
�MB
p�B
U�B
>BB

�B	��B	ѷB	��B	��B	�bB	o B	_;B	^5B	aHB	x8B	��B	�\B	��B	��B	�
B
�B
-CB
IRB
T�B
g�B
��B
��B
��B
�B
�B
�LB
�FB
��B
��B
˒B
��B
՛B
�)B
�B
��B
�B
��B
�B
�EB
�yB
��B
ӏB
��B
��B
֡B
�B
�9B
�B
ܒB
�#B
רB
�gB
�2B
�B
��B
�aB
� B
��B
�?B
��B
�[B
�CB
�qB
�*B
��B
��B
�!B
��B
�	B
��B
�B
��B
�:B
��B
��B
�tB
�tB
��B
��B
�XB
�B
��B
��B
��B
��B
�tB
�3B
�9B
��B
��B
��B
�$B
�*B
��B
��B
��B
��B
�LB
�B
��B
�B
�B
�zB
�?B
�9B
��B
��B
��B
�kB
��B
�:B
��B
��B
��B
��B
�CB
��B
��B
�_B
�B
��B
�.B
��B
�VB
��B
��B
�1B
��B
�B
�B
�B
|B
{JB
x8B
rB
poB
qvB
sMB
r�B
r�B
sB
poB
o B
m�B
l�B
kB
jKB
iDB
h
B
c�B
bB
aB
\�B
YKB
[#B
U�B
VmB
XEB
\]B
]/B
^5B
_pB
\�B
\�B
]/B
\�B
\�B
[�B
[�B
[WB
[�B
[#B
Z�B
Y�B
X�B
W�B
W
B
VmB
VB
T�B
R�B
P�B
OBB
M�B
I�B
H�B
H�B
GB
FtB
DgB
DgB
C�B
AUB
A�B
@B
>wB
>BB
=B
<�B
;�B
:�B
9�B
9�B
7�B
6zB
5?B
3hB
2aB
0!B
.B
,B
,qB
,B
*�B
(�B
$@B
$tB
#:B
$@B
#nB
"�B
"�B
!�B
!�B
!�B
!�B
!�B
 �B
 'B
�B
�B
OB
~B
CB
�B
�B
�B
�B
�B
�B
�B
7B
eB
1B
�B
�B
1B
B
�B
�B
�B
�B
(B
�B
(B
�B
�B
�B
�B
(B
DB

�B

rB

	B
	�B
�B

�B

�B
DB
JB
B
�B
�B
�B
 B
hB
�B
:B
:B
FB
FB
B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
SB
�B
B
�B
�B
�B
�B
�B
SB
SB
SB
SB
�B
�B
�B
�B
bB
hB
B
�B
oB
�B
uB
�B
B
B
B
�B
MB
SB
$B
YB
�B
�B
�B
B
B
�B
�B
B
eB
�B
�B
kB
�B
�B
�B
�B
=B
xB
B
CB
�B
IB
�B
�B
�B
�B
!B
�B
VB
�B
�B
VB
VB
�B
 \B
�B
�B
�B
 �B
 �B
!bB
"hB
#:B
#�B
#nB
$@B
$�B
%FB
$�B
&B
'B
'�B
'�B
'�B
'�B
'RB
'�B
(�B
(�B
(�B
)*B
(�B
(�B
)_B
)�B
)_B
)_B
)�B
)�B
*0B
)�B
*0B
)�B
*eB
*�B
+6B
+6B
+kB
,B
-CB
,�B
,qB
.B
-�B
-�B
-wB
-�B
-wB
-�B
-�B
-�B
-�B
-�B
-B
-CB
-�B
.B
.B
-�B
.B
.�B
.�B
.�B
/OB
.}B
/OB
.�B
/B
/�B
/�B
/OB
/B
.�B
.�B
.}B
.�B
/�B
/�B
0UB
/�B
0UB
0!B
/�B
0�B
1[B
1�B
2�B
3hB
3�B
3hB
3�B
3�B
4B
4B
49B
5?B
4�B
5B
5?B
5?B
5B
5?B
6FB
6zB
7B
6�B
6�B
6�B
7LB
7�B
8B
8B
7�B
8RB
9$B
8�B
8�B
9�B
:�B
:�B
;0B
;�B
;dB
;0B
;0B
<6B
<6B
<jB
<6B
<jB
<6B
<6B
=�B
=qB
>BB
=�B
>BB
>�B
?B
>�B
>�B
>�B
?�B
?�B
@B
@OB
@B
@B
@�B
@B
@B
@�B
A B
@�B
A�B
A�B
B'B
B�B
B[B
B�B
B[B
B�B
C�B
C�B
C�B
D3B
DgB
DgB
DgB
EB
D�B
FB
FB
F?B
GB
GB
GEB
GEB
GEB
GEB
G�B
G�B
HKB
H�B
I�B
I�B
I�B
I�B
I�B
J#B
J#B
J#B
J�B
J�B
J�B
J�B
L0B
L�B
L�B
NpB
NB
NB
OBB
OvB
PHB
PB
PHB
P�B
QNB
Q�B
RTB
R�B
S�B
S�B
S�B
S�B
T,B
U2B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
U2B
U�B
V9B
XEB
XEB
X�B
X�B
XyB
X�B
X�B
X�B
X�B
X�B
YB
YB
X�B
YKB
Y�B
Z�B
[#B
Z�B
Z�B
Z�B
[#B
[#B
\)B
\]B
\�B
]/B
]dB
]dB
]�B
]�B
]�B
^5B
^jB
^�B
^�B
^jB
^�B
^jB
^�B
^�B
^�B
_�B
_�B
_�B
_�B
`B
_�B
_�B
_pB
`�B
`�B
aB
aHB
aHB
aHB
aHB
aB
aB
a|B
a|B
a|B
a�B
bB
bB
bB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c B
c B
c B
c B
b�B
b�B
d&B
c�B
d�B
d�B
d�B
d�B
d�B
dZB
dZB
d&B
dZB
d�B
d�B
e�B
e�B
e�B
ffB
f2B
f�B
ffB
f2B
ffB
ffB
ffB
ffB
gB
f�B
g�B
gmB
g8B
g�B
g�B
g�B
h
B
g�B
g�B
h
B
h>B
hsB
hsB
h�B
h�B
iB
iB
iB
h�B
iDB
i�B
jKB
jB
i�B
jB
jB
j�B
j�B
kB
kQB
kQB
kB
kQB
k�B
k�B
k�B
k�B
lWB
lWB
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
l�B
m)B
m�B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
m�B
n/B
n/B
n�B
n�B
o5B
o5B
o�B
o�B
o�B
o�B
p;B
p�B
p�B
p�B
p�B
p�B
p�B
qB
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r|B
r�B
sB
s�B
s�B
s�B
sMB
s�B
s�B
tTB
tTB
t�B
t�B
t�B
tTB
uZB
u�B
u�B
v+B
u�B
v`B
v�B
v�B
v�B
wfB
wfB
x8B
xlB
x�B
y	B
y>B
y>B
yrB
yrB
yrB
zxB
zxB
zxB
z�B
z�B
z�B
{B
{�B
{�B
{�B
|B
|�B
|�B
|�B
|�B
|�B
|�B
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
cB
.B
.B
cB
cB
�B
�B
� B
�4B
��B
�;B
�B
�B
�B
�;B
�oB
��B
��B
�B
�B
�AB
�B
�uB
��B
��B
�B
��B
�MB
��B
��B
��B
��B
��B
��B
�SB
�SB
��B
��B
��B
��B
��B
�%B
�YB
��B
��B
��B
��B
��B
��B
�YB
�_B
�_B
��B
�fB
�1B
�fB
�B
�lB
�lB
�lB
�lB
��B
��B
��B
�B
�xB
��B
�xB
�DB
�xB
��B
��B
��B
�B
�B
�JB
�JB
�~B
�~B
��B
��B�5B�B� B�AB��B�B�B��B�iB�B�;B�;B�WB�B��B�B�;B��B��B��B�5B�;B�B�cB�cB�iB�;B��B�;B�iB�oB�oB�B��B�5B�;B��B�B��B�B�iB�B�B�vB�5B� B��B��B�oB�;B�5B�B��B�B�;B�iB��B�oB��B�B�B��B�ZB	AB	 �B	3hB	8�B	v`B	�0B	�B	�dB	��B	��B	��B	ϫB	��B	��B
bB
8�B
C�B
K^B
^�B
u%B
�;B
�GB
�B
�B
��B
��B
�MB
�7B
�=B
�~B
�'B
��B
�bB
��B
��B
�VB
��B
��B
�'B
��B
��B
��B
��B
��B
�LB
��B
�RB
�B
��B
�nB
�:B
��B
�4B
��B
�\B
��B
�-B
�hB
�4B
�4B
��B
��B
��B
��B
�:B
�:B
��B
��B
�-B
�B
��B
��B
��B
�VB
��B
��B
��B
�IB
��B
�kB
�_B
��B
�YB
�B
��B
�~B
�B
��B
� B
�IB
�VB
��B
�B
�GB
�YB
��B
�:B
}"B
�xB
r|B
t�B
s�B
u%B
�@B
�*B
�B
��B
XB
HB
A�B
5�B
XyB
W�B
V�B
T,B
\]B
a�B
��B
�WB
��B
�<B
��B
�OB
�tB
�B
�}B
��B
�B
��B
�[B
�&B
��B
�9B
֡B
רB
��B
�B
�B
�B
��B
�oB
��B
�]B
�B�B	lB�B
�]B
�JB
�cB
�B
��B
�jB
�B
��B
��B
��B
��B
�EB
�5B
ޞB
��B
�AB
�B
��B
�B
��B
�B
�xB
�JB
�B
��B
��B
��B
��B
��B 4BB�B�B+B1B�B�B�B�B�B	7B
=B�B�B{B4BhB�BuB�B'�B!B �B#B$@B"�B�B�B�BB \B!�B�B �B&�B&�B%zB$�B$�B*eB-wB+�B.}B,=B,�B2aB4B)_B(�B+�B33B4�B2aB.�B-wB-�B,�B,qB,�B,B,�B.�B1[B/�B,�B+�B,B+B5?B:�B=�B@�BFtBGzBPBC-BF?BC-B=<B7LB5�B4�B1�B-�B9$B>BB>�B=�B=�B>wB>wB>wB@OB;�B6�B5�B6�B4�B5B49B49B33B33B33B33B3hB4�B2�B2�B4nB4nB4�B5�B5�B<B:^B@�BJ�BVmBd�Be�Be`Be`Bg�Bj�Bi�Bg�Be�Bi�Bu�Bs�Be`BXyBNpBU2BZBV�BV�BWsBW�BXyB\)Bf�Bg�Bg8Bc�B\�B]�BYBXyBYBYKBXyBZ�B_;BTaBQ�BQ�BNpBNpBM�BL�BL0BM�BJ�BIBFtBC�BCaBA�BB[B@�B=�B7�B8RB9�B.B,=B*�B+6B)�B!�B	BCB�B�B�B�B.}B,B<jBA�B?B:^B6B5�B4�B1[B.�B0UB-B'�B+�B,�B"hB$B*�B�B�B\B	�BuB;B1B	�BuB
��B
��B
�B
�NB
��B
�#B
�pB
چB
�iB
ӏB
�jB
�jB
�vB
��B
�jB
ȀB
��B
��B
�$B
�$B
��B
��B
��B
�B
��B
��B
��B
�_B
�!B
��B
�YB
�1B
�uB
��B
��B
��B
�~B
�PB
��B
��B
�{B
�B
�B
~(B
x�B
v�B
~�B
qvB
k�B
g8B
gB
h�B
f�B
\)B
T�B
P�B
L�B
L�B
?}B
?�B
8�B
6B
9$B
:�B
R�B
JXB
�B

�B
B
-�B
4B
�B
  B	�cB	��B	�KB	��B	ߤB	ܒB	��B	�B	�BB	��B	�jB	ΥB	چB	��B	��B	�RB	�B	�3B	��B	��B	�CB	�=B	�B	�0B	�_B	��B	�B	�:B	�:B	��B	�B	�B	��B	�XB	�_B	�"B	�VB	��B	}VB	�B	��B	�_B	o�B	p�B	n�B	k�B	iyB	j�B	d�B	i�B	m]B	o5B	i�B	YB	XB	V�B	V9B	W�B	XyB	[�B	_;B	_;B	_�B	_�B	]�B	_;B	]�B	_pB	_�B	`BB	_�B	c�B	d�B	g�B	b�B	gmB	h�B	c�B	iB	jB	v�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�bB	�bB	�\B	�4B	�hB	�eB	�tB	�BB	�XB	��B	��B	ƨB	�EB	��B	̘B	�<B	��B	��B	��B	�[B	��B	�KB	�B	�B	�B	��B	��B
�B
�B
	B
�B
uB
�B
*0B
&�B
%�B
(�B
,�B
/B
0�B
1[B
1�B
0�B
G�B
GB
F?B
^�B
OvB
B[B
>B
DgB
\�B
Z�B
[�B
[WB
b�B
b�B
d&B
g�B
c�B
i�B
r�B
y>B
�_B
��B
�4B
��B
�\B
�.B
��B
�:B
��B
�1B
��B
�B
�B
�-B
�tB
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�6B
�?B
��B
��B
��B
��B
�}B
�!B
��B
��B
�<B
�-B
��B
�B
�$B
�9B
�3B
��B
�FB
�$B
��B
��B
��B
��B
�BB
��B
�wB
�qB
�KB
��B
��B
��B
ʌB
ΥB
ʌB
��B
�RB
�gB
�?B
�0B
�dB
�6B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                B�PB�B�B��B��B��B�B�B�PB�B�B�B�PB�B�B	r�B	�]B
<�B
�B
��B
�BB
��B
�aB
��B
�B
�B
��B
�BB
�RB
��B
�CB
��B
��B
SfB
P�B
�#B
��B
�B
�VB
��B
�qB�BtB�B#B)B'�B4�B3sB6�B.TB0,B\4BZ�BX�BS�BH�B?�B-NBB0�B'�B�B
�'B
��B
�WB
�8B
��B
�OB
}�B
j�B
O�B
7�B
XB	�{B	�iB	�dB	�BB	�B	h�B	X�B	W�B	Z�B	q�B	�BB	�B	��B	ȋB	�B
�B
&�B
CB
N|B
a�B
�^B
�?B
��B
��B
��B
��B
��B
��B
��B
�DB
B
�MB
��B
�cB
��B
�4B
ӚB
��B
��B
�+B
�rB
�AB
�|B
ΰB
�SB
϶B
��B
�1B
�DB
��B
�ZB
�B
��B
϶B
ΰB
�B
��B
�B
��B
��B
�B
��B
�#B
��B
��B
��B
��B
�RB
��B
�6B
��B
�UB
��B
��B
��B
�&B
�&B
�pB
��B
�
B
��B
��B
�`B
�ZB
��B
�&B
��B
��B
�NB
��B
�mB
��B
��B
�ZB
�NB
�ZB
�2B
��B
��B
��B
��B
��B
�,B
��B
��B
�NB
�5B
��B
�B
�?B
��B
�BB
�6B
��B
��B
��B
��B
��B
�B
��B
��B
��B
�CB
�B
��B
�LB
��B
�LB
}�B
{�B
y~B
u�B
t�B
q�B
k�B
j!B
k(B
l�B
lbB
l�B
l�B
j!B
h�B
gCB
frB
d�B
c�B
b�B
a�B
]�B
[�B
Z�B
V�B
R�B
T�B
O�B
PB
Q�B
VB
V�B
W�B
Y"B
VxB
V�B
V�B
V�B
VxB
U�B
U>B
U	B
UrB
T�B
T�B
SfB
R`B
Q�B
P�B
PB
O�B
NGB
LoB
JcB
H�B
GQB
C�B
BfB
BfB
@�B
@&B
>B
>B
=�B
;B
;pB
9�B
8)B
7�B
6�B
6�B
5B
4EB
3sB
3�B
1gB
0,B
.�B
-B
,B
)�B
'�B
%�B
&#B
%�B
$�B
"sB
�B
&B
�B
�B
 B
NB
NB
}B
}B
�B
}B
}B
�B
�B
6B
6B
B
0B
�B
�B
�B
XB
XB
�B
XB
RB
�B
B
�B
�B
�B
�B
�B

IB
	�B
	wB
	CB
�B
qB
�B
	�B
kB
<B
6B
�B
�B
�B
$B
�B
�B
^B
XB
�B
�B
�B
�B
�B
OB

IB

�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
aB
�B
�B
aB
aB
aB
aB
�B
�B
�B
aB
�B
�B
�B
-B
-B
B
�B
�B
nB
9B
nB
9B
9B
B
B
B
B
nB
aB
�B
OB

B
B
�B
UB
!B
�B
'B
�B
�B
�B
�B
3B
�B
B
�B
B
zB
�B
LB
�B
�B
�B
�B
�B
B
�B
�B
B
�B
RB
RB
�B
�B
*B
�B
�B
^B
�B
dB
6B
�B
�B
�B
<B
B
�B
�B
B
B
<B
B
�B
pB
pB
BB
�B
B
B
�B
UB
 B
�B
[B
�B
�B
�B
 �B
!9B
!9B
!mB
!9B
!B
!mB
"sB
"sB
"�B
"�B
"�B
"�B
#B
#EB
#B
#B
#yB
#�B
#�B
#�B
#�B
#�B
$B
$KB
$�B
$�B
%B
%�B
&�B
&WB
&#B
'�B
'�B
'^B
')B
'^B
')B
'�B
'^B
'�B
'�B
'^B
&�B
&�B
'^B
'�B
'�B
'�B
'�B
(dB
(dB
(dB
)B
(/B
)B
(�B
(�B
)5B
)jB
)B
(�B
(�B
(dB
(/B
(dB
)jB
)jB
*B
)jB
*B
)�B
)�B
*�B
+B
+vB
,�B
-B
-NB
-B
-NB
-�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
/�B
0,B
0�B
0�B
0�B
0�B
0�B
12B
1�B
1�B
1�B
2B
2�B
2mB
2mB
3�B
4yB
4�B
4�B
5KB
5B
4�B
4�B
5�B
5�B
6B
5�B
6B
5�B
5�B
7WB
7#B
7�B
7�B
7�B
8�B
8�B
8]B
8�B
8�B
9�B
9cB
9�B
:B
9�B
9�B
:�B
9�B
9�B
:�B
:�B
:jB
;�B
;;B
;�B
<AB
<B
<AB
<B
<vB
=HB
=|B
=�B
=�B
>B
>B
>B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
A`B
A`B
A�B
B�B
C8B
C8B
C8B
C8B
C8B
C�B
C�B
C�B
D�B
D�B
D�B
D>B
E�B
FB
FJB
H"B
G�B
G�B
H�B
I(B
I�B
I�B
I�B
J�B
K B
KiB
LB
L�B
MAB
M�B
M�B
M�B
M�B
N�B
NGB
N|B
N|B
N|B
N�B
N|B
N|B
N�B
O�B
O�B
Q�B
Q�B
R`B
R`B
R+B
R`B
R�B
R�B
R`B
R�B
R�B
R�B
R�B
R�B
SfB
TlB
T�B
T�B
T�B
T�B
T�B
T�B
U�B
VB
V�B
V�B
WB
WB
WJB
WJB
W~B
W�B
XB
XPB
XPB
XB
XPB
XB
XPB
XPB
X�B
YVB
Y�B
Y�B
Y�B
Y�B
Y�B
YVB
Y"B
Z\B
Z\B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[.B
[.B
[.B
[�B
[�B
[�B
[�B
\4B
\iB
\4B
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
]�B
]�B
^AB
^AB
^AB
^AB
^AB
^B
^B
]�B
^B
^AB
^uB
_{B
_�B
_�B
`B
_�B
`�B
`B
_�B
`B
`B
`B
`B
`�B
`�B
aSB
aB
`�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
b%B
b%B
bYB
b�B
b�B
b�B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
d1B
deB
d�B
d�B
eB
eB
d�B
eB
e7B
e7B
e7B
e7B
f	B
f	B
f	B
f	B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gCB
gB
gxB
g�B
g�B
g�B
gxB
g�B
g�B
g�B
g�B
g�B
hJB
h~B
h�B
h�B
iPB
iPB
i�B
iPB
i�B
j�B
jVB
jVB
j�B
jVB
j�B
j�B
k\B
k\B
k\B
k�B
k\B
k�B
k\B
k\B
l.B
l�B
l�B
m4B
m4B
m4B
l�B
mhB
m�B
nB
nB
n:B
n:B
n:B
nB
oB
ouB
o�B
o�B
o�B
pB
p�B
p�B
p�B
qB
qB
q�B
rB
rSB
r�B
r�B
r�B
s$B
s$B
s$B
t*B
t*B
t*B
t_B
t�B
t�B
u1B
u�B
ueB
u�B
u�B
v7B
v7B
v7B
v7B
vkB
v7B
w=B
wB
wqB
wqB
wqB
w�B
xB
xwB
x�B
x�B
yB
x�B
x�B
yB
yB
yIB
y~B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{!B
{UB
{�B
{�B
{�B
{�B
{�B
|'B
|\B
|�B
|�B
}bB
}�B
~hB
~�B
~�B
~�B
~�B
~�B
B
B
:B
nB
nB
nB
nB
�B
�B
�@B
�tB
�@B
�tB
�@B
�@B
�B
�B
�B
�FB
�B
��B
�B
��B
�B
�B
�B
�B
�RB
�XB
�XB
��B
�*B
�^B
�*B
��B
�*B
�^B
�^B
�^B
��B
��B
��B
��B
�0B
�0B
�eB
��B��B�JB�B��B�B�VB�JB�~B�B�VB��B��B�	B�JB�B�PB��B�B�~B�xB��B��B�B�B�B�B��B�B��B�B�!B�!B�PB�~B��B��B�B�JB�~B�JB�B�PB�B�(B��B�B�~B�B�!B��B��B�JB�B�B��B�B�~B�!B�B�VB��B�B�B��B	�B	-B	28B	pB	��B	��B	�B	��B	��B	�]B	�]B	�B	�qB

B
2mB
=�B
EB
XPB
n�B
z�B
|�B
~�B
��B
�^B
�IB
��B
��B
��B
�0B
��B
��B
�B
�HB
�<B
�B
�pB
�wB
��B
�pB
�<B
�BB
�}B
��B
��B
�9B
�B
��B
��B
� B
��B
�NB
��B
�BB
�B
��B
��B
�B
��B
��B
��B
��B
�}B
��B
��B
��B
�}B
�wB
��B
��B
�NB
��B
��B
�B
�<B
��B
��B
��B
�RB
�B
�B
��B
�B
��B
��B
�0B
��B
�6B
��B
��B
�B
~hB
��B
|�B
�B
�wB
��B
v�B
�*B
l.B
n:B
m�B
n�B
��B
��B
��B
�[B
Q�B
A�B
;�B
/�B
R+B
Q�B
P�B
M�B
VB
[cB
ħB
�	B
�|B
��B
�EB
�B
�&B
϶B
�/B
ȋB
��B
̤B
�B
��B
ͪB
��B
�SB
�ZB
էB
�MB
�1B
�lB
�B
�!B
�~B
�B
�hB
�3BB
�hB
�B
��B
�B
�JB
��B
�B
ٿB
�uB
�B
ڑB
�rB
��B
��B
�PB
��B
��B
��B
�B
�B
�_B
��B
�*B
��B
�1B
��B
�=B
��B
�=B
�~B
��B
��B
��B
��B �B�B�BLBFBFB�B�B�B�B
}B-B
�BBOB'BzB!9B�BBB�B�B�BpB�BdB�BB�B�BwB gB 3B,B[B�B$B')B%QB(/B%�B&�B,B-�B#B"sB%�B,�B.�B,B(dB')B'^B&�B&#B&�B%�B&WB(dB+B)5B&WB%QB%�B$�B.�B4�B7WB:jB@&BA,BI�B<�B?�B<�B6�B0�B/ZB.TB+vB'^B2�B7�B8]B7WB7WB8)B8)B8)B:B5KB0�B/ZB0`B.TB.�B-�B-�B,�B,�B,�B,�B-B.TB,�B,HB. B. B.TB/�B/ZB5�B4B:5BD�BPB^�B_{B_B_Ba�Bd�Bc�BaSB_�Bc_BouBm4B_BR+BH"BN�BS�BPSBPSBQ%BQZBR+BU�B`MBaSB`�B]�BVxBW~BS1BR+BS1BR�BR+BT8BX�BNBK�BK5BH"BH"BGQBFBE�BGQBDsBB�B@&B=�B=B;�B<B:5B7�B12B2B3�B'�B%�B$�B$�B#yBHB�B�B�BdB^BpB(/B%�B6B;pB8�B4B/�B/ZB.TB+B(dB*B&�B!�B%�B&WBB�B$KBzB[B	B�B
�'B
��B�B�B
�'B
�uB
�nB
�lB
� B
էB
��B
�"B
�8B
�B
�AB
�B
�B
�(B
ɑB
�B
�2B
�]B
�KB
��B
��B
�mB
�HB
��B
��B
�yB
�}B
�wB
�B
��B
��B
�B
��B
�'B
��B
�<B
�qB
�0B
�B
��B
~hB
}-B
y~B
{�B
w�B
r�B
pFB
xCB
k(B
e7B
`�B
`�B
bYB
`�B
U�B
N|B
JcB
FJB
FB
9/B
9cB
2mB
/�B
2�B
4�B
L�B
D
B
9B
�B
�B
'�B

�B
6B	��B	�B	�B	��B	�YB	�VB	�DB	ԠB	�1B	��B	ʗB	�B	�WB	�8B	ݣB	�5B	�B	��B	��B	�5B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�wB	��B	��B	�gB	�
B	�B	��B	�B	{�B	wB	��B	�<B	�B	i�B	jVB	hJB	e7B	c+B	d�B	^�B	c_B	gB	h�B	c_B	R�B	Q�B	PSB	O�B	Q�B	R+B	U�B	X�B	X�B	Y�B	Y�B	W~B	X�B	W~B	Y"B	Y�B	Y�B	YVB	]�B	^�B	aSB	\4B	aB	b�B	]�B	b�B	d1B	p�B	��B	�wB	�6B	�CB	��B	��B	��B	�wB	�jB	�6B	�jB	��B	�<B	��B	�wB	�B	�B	�B	��B	�B	�B	�&B	��B	�
B	��B	��B	�ZB	��B	áB	�JB	��B	ȋB	ɑB	ͪB	�B	юB	��B	׳B	��B	�1B	�@B	��B
�B
�B
�B	��B
'B
jB
#�B
 gB
aB
"sB
&WB
(�B
*<B
+B
+�B
*pB
A`B
@�B
?�B
X�B
I(B
<B
7�B
>B
VxB
T�B
UrB
U	B
\4B
\iB
]�B
aSB
]oB
c_B
l�B
r�B
�B
�^B
��B
�qB
�B
��B
�[B
��B
��B
��B
��B
��B
��B
��B
�&B
�5B
��B
�^B
��B
�^B
�dB
��B
��B
��B
�QB
�vB
��B
��B
�|B
��B
��B
�dB
�/B
��B
��B
�vB
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
�mB
��B
��B
�|B
��B
��B
�)B
�#B
��B
ŭB
�sB
ħB
�>B
�WB
�>B
��B
�B
�B
��B
��B
�B
��B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223247                            20230426223247AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622324720230426223247  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622324720230426223247QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622324720230426223247QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               