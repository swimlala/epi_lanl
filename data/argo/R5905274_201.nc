CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-10T07:01:06Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230710070106  20230710070106  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�6�\/�@�6�\/�11  @�6�DDKP@�6�DDKP@0��GG�@0��GG��d%��F4��d%��F4�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  AA  ?u@�\@=p�@�G�@��\@�G�@�p�A   A��A\)A*�HA?\)A_\)A\)A�  A�Q�A�  A�Q�AУ�A�\)A�B Q�Bz�B  B�
B�
B(  B0(�B8(�B@  BH  BPQ�BXQ�B`  Bh  Bp  Bw�
B�  B�{B�(�B�  B��B�{B�  B�  B�(�B�  B��B�  B�{B�  B��B�{B��B�{B�(�B�{B�  B�  B�{B�  B�(�B�(�B�(�B�(�B�{B�(�B�(�B�{C   C{C{C  C  C

=C{C{C  C
=C
=C
=C  C  C��C��C��C!��C#��C%�C(  C*{C,{C.
=C0{C1��C3��C5��C7�HC9��C<{C=��C?�CA��CD  CF
=CH{CJ�CL{CM�CP  CR{CS��CV{CX{CZ
=C\
=C^
=C`�Cb
=Cc��Ce��Ch  Cj  Ck��Cn  Cp{Cr  Ct  Cv  Cx  Cz
=C|  C~  C�C�  C�C�  C�C�  C���C���C���C�C�
=C�C�  C���C�  C�C�C�C�  C�  C�  C�C�
=C�  C���C�  C�C�
=C�  C���C�  C�C�C�C�C���C���C�  C�  C�C�
=C�C�
=C�C�
=C�  C���C���C���C�  C�  C�C�
=C�
=C���C���C�  C�  C�  C�  C�C�C�
=C�
=C�  C�C�C�  C�  C���C�C�\C�C���C�  C�  C�  C���C�C�  C���C���C���C�  C���C���C�
=C�C�  C�
=C�\C�  C��C��C���C���C���C�  C�  C�  C���C���C�  C���C���C���C���C���C�  C�  C�  C�C���C���C���C�C�
=C�  C�C�C�  C���C���C���C���C�  C�
=C�D �D � D�Dz�D��D��D�D}qD��D}qD��Dz�D�qD��DD��D�D��D	  D	z�D	�qD
}qD
��D}qD  D� D�qDz�D�qD�DD}qD  D}qD  D}qD  D��D�D� D�qDz�D�qD}qD��Dz�D��D� D  D� D�D��DD� D��D� D�D� D�qD��D  D}qD  D� D�qD � D!  D!}qD"�D"��D#  D#� D$  D$��D%�D%� D%�qD&��D'�D'� D(  D(� D(�qD)}qD)�qD*}qD+  D+� D,�D,��D-�D-��D.�D.}qD.�RD/z�D0  D0��D1  D1z�D2  D2��D2�qD3}qD3�qD4� D5  D5�D6
=D6��D6�RD7xRD7�qD8� D9D9��D:D:��D;  D;� D;�qD<z�D<��D=}qD>�D>� D>��D?xRD?��D@}qD@��DA}qDB  DB� DC  DC��DD�DD}qDD�qDE}qDE�qDF�DG�DG�DH�DH��DI�DI��DJDJ��DK  DK��DL  DL��DMDM��DN�DN� DO�DO� DO��DP� DQ�DQ� DR  DR��DS�DS}qDS��DT� DU�DU�DV  DV}qDW�DW�DX�DXz�DX�qDY� DY�qDZ}qD[�D[��D\  D\� D\�qD]}qD]�qD^��D_�D_��D`�D`��Da�Da��Db  Db��Dc  Dcz�Dc�qDd� De  De}qDe��Df}qDg  Dg��Dh  Dh}qDi�Di�Dj  Dj}qDj��Dk}qDk�qDl� Dm�Dm� Dm�qDn}qDo  Do}qDo�qDp��Dq  DqxRDq��Dr��Ds  Ds}qDt�Dt� Du  Du�DvDv}qDv�qDw� Dw�qDx� Dy  Dy� Dy�qDz� D{D{�D{�qD|� D}  D}}qD~�D~��D  D}qD��D�>�D�� D�� D���D�>�D�� D�� D�  D�B�D�� D���D���D�>�D�� D��HD�  D�@ D�� D�� D�  D�=qD�~�D��qD���D�AHD��HD���D���D�AHD�� D�� D���D�>�D��HD�D�  D�@ D�~�D���D�  D�AHD�~�D���D���D�AHD�� D���D�HD�>�D�� D���D��qD�>�D�� D��HD�  D�>�D�~�D�� D�  D�AHD��HD��HD�  D�>�D�� D���D���D�>�D�~�D���D���D�>�D�}qD�� D��D�AHD��HD��qD�  D�B�D�~�D�� D��D�B�D��HD�� D�HD�AHD�� D��HD�  D�>�D�� D���D�  D�AHD��HD�� D�HD�@ D�~�D��HD��D�AHD�� D���D�HD�AHD��HD�� D�  D�AHD��HD�� D��qD�>�D�~�D��qD�  D�AHD�� D�� D�HD�AHD�}qD���D�  D�@ D�� D�� D�HD�AHD�~�D���D�HD�@ D�~�D��qD���D�AHD�� D���D���D�@ D�� D���D�HD�AHD�� D��qD��qD�@ D���D�D�  D�@ D���D��HD�HD�C�D���D���D��qD�@ D�~�D��qD���D�AHD�~�D���D�HD�AHD�~�D��)D��)D�<)D�� D���D��D�ED���D�� D�  D�AHD�~�D��qD���D�AHD���D��HD���D�>�D�}qD�� D�HD�B�D��HD�� D�  D�B�D���D�� D��D�AHD�}qD��)D���D�=qD�}qD�� D��D�B�D�~�D���D���D�=qD��HD�� D���D�B�D��HD��HD�HD�=qD�}qD�� D��D�B�D�~�D���D��D�B�D��HD�D���D�>�D��HD��HD��qD�=qD�� D��HD��qD�=qD�� D�� D�  D�AHD�~�D��qD��qD�>�D��HD���D�  D�B�D���D�� D��qD�>�D�}qD��HD��D�@ D�~�D¾�D�  D�@ DÀ D�� D�HD�AHDĀ D�D��D�@ Dŀ D��HD�HD�@ D�~�D�� D�HD�>�D�~�D��HD�HD�>�DȁHD��HD�  D�@ D�~�Dɾ�D���D�@ D�~�Dʾ�D�  D�@ Dˀ D˾�D�HD�@ D�}qD̾�D���D�@ D̀ D;�D�HD�>�D�}qD�� D�  D�=qDπ D�� D���D�AHDЀ Dо�D�HD�@ Dр D�D�HD�@ DҀ DҾ�D�  D�B�DӁHD�� D�  D�>�D�~�D�� D�  D�>�DՁHD��HD�HD�@ D�~�D��HD�  D�@ D�~�D�� D�HD�AHD؁HD��HD�  D�>�DفHD�� D���D�@ D�~�D�� D�HD�@ DہHD�� D��qD�>�D܀ D�D��D�AHD݀ D�� D�  D�@ Dހ D�� D�  D�AHD߀ D�� D��D�@ D�~�DྸD��qD�@ D�HD�� D�  D�>�D�~�D��HD�HD�@ D� D㾸D�HD�AHD�HD�� D���D�=qD�}qD�qD��qD�=qD�~�D澸D�  D�AHD�HD��HD�  D�>�D�}qD�qD��qD�>�D�}qD龸D�HD�@ D�}qD�� D�HD�>�D� D뾸D���D�@ D�HD�� D�  D�@ D�}qD���D���D�=qD�HD��HD�HD�AHD� D�D���D�>�D�~�D�D���D�>�D� D�D��)D�@ D� D�� D�HD�AHD�HD�� D�HD�AHD� D��qD���D�@ D�� D���D�  D�@ D�~�D��qD���D�>�D��HD�D�HD�@ D�� D�� D�  D�@ D�~�D���D��qD�=qD�|)>#�
?\)?W
=?���?���?��H@(�@0��@J=q@k�@}p�@�\)@��R@�ff@�Q�@\@�33@��H@�=q@��HA�A
�HA�AffA�RA#33A*�HA2�\A7
=A?\)AC33AI��AQG�AU�A]p�AaG�Ai��Amp�AuA|(�A�  A�(�A�ffA�=qA�z�A�Q�A��A�A�G�A��A�\)A�G�A�p�A�\)A�=qA�A��A��A�A�G�A��A�
=A�G�A�p�AǮA��
A�ffAҏ\A�p�A�Q�A�z�A�
=A�33A�p�A�=qA��A�  A�z�A��RA��\A�{B ��B�RB�
B�B
=B��B
�\B�Bp�B=qB  B��B�\B\)Bz�BB�\B  B��BffB�HB(�Bp�B=qB�
B z�B!�B"�RB$(�B$��B%B'\)B(  B)��B*�\B+33B,��B-p�B/
=B/�B1G�B2{B3
=B4z�B5�B6�HB7�B8z�B:{B:�\B;�B=G�B=�B?�B@z�BAG�BB�HBC�
BDz�BF{BF�RBG�
BIG�BJ{BK33BL��BMG�BNffBO�
BPz�BR{BS33BS�
BUp�BU�BW
=BX��BYG�BZ=qB[�
B\Q�B]��B_
=B_�Ba�Ba��Bc33Bd(�Bd��BfffBf�HBh(�Bip�Bj{Bk�Bl(�Bm��Bn{Bo33Bp��BqG�BrffBs�BtQ�BuBvffBw\)Bx��ByG�Bz�RB{�
B|z�B~{B~�HB�B��\B��HB�G�B�  B�=qB��RB��B�B�z�B���B�G�B��B�(�B��HB�\)B���B�Q�B��\B�
=B��B�  B��\B��B�\)B��B�z�B��RB�G�B��
B�  B���B���B�p�B�  B�=qB���B�G�B���B�=qB�ffB��HB���B��
B�ffB�
=B�G�B��B�z�B��RB�\)B��B�(�B���B�\)B���B�{B��HB�G�B���B�Q�B���B�
=B��
B�Q�B��RB�p�B�  B�=qB�
=B���B��B��RB��B���B�Q�B��HB�33B�  B��\B��HB��B�=qB���B�p�B��
B�Q�B�33B��B�  B���B�33B��B��\B��HB�p�B�=qB���B��B��B�Q�B��RB��B��B��\B�G�B���B�Q�B�
=B�\)B�  B���B�\)B�B��\B�33B��B�(�B��HB��B��
B�ffB�G�B�B�{B��RB��B�{B�z�B��B��
B�ffB���Bř�B��BƏ\B�p�B�  B�Q�B���B�B�=qBʸRB�p�B�(�B̸RB�33B�  BΏ\B��HBϙ�B�ffB���B�\)B�(�BҸRB�G�BӮB�Q�B�33B�B�(�B���Bי�B�(�Bأ�B�33B�  Bڣ�B�
=Bۙ�B�z�B�
=B�\)B�{B���B߅B��B�z�B�33B��B�Q�B��HB�B�Q�B��HB�G�B��B���B�\)B�B�ffB�33B��
B�z�B���B뙚B�=qB��B��B�Q�B�RB�\)B�=qB��HB�B��
B��B�33B�(�B���B�33B��B��\B�\)B�(�B���B��B��B��RB�\)B�=qB���B���B�{B���B�\)C 
=C ffC ��C33C�CC�Cp�C�
CG�C��C��C(�C�C�CG�C�RC
=CffC�RC  CQ�C�RC
=C�C�
C	33C	z�C	C
(�C
z�C
��CQ�C�C��CG�C�C  C�C�HC=qC��C�HC33C�\C��Cp�C��C(�C�C�C33C�C�HCG�C��C{Cz�C�
C33C�C�
C(�C�C�HC=qC�\C��CQ�CC(�C�C�HC=qC��C��CG�C��C��C\)CC�C�\C  CffC��C33C�C�C Q�C �C!  C!Q�C!�RC"{C"z�C"�
C#G�C#C$�C$�C$�HC%33C%��C&  C&\)C&C'�C'��C(
=C(p�C(��C)33C)�\C)�HC*33C*��C+  C+\)C+�RC,�C,�\C-  C-ffC-�RC.�C.�C.�
C/33C/�C/�C0=qC0�C1
=C1p�C1�
C2=qC2��C3  C3p�C3�HC433C4��C4��C5\)C5�RC6
=C6p�C6C7(�C7�\C7��C8Q�C8�RC9�C9�C9��C:ffC:�
C;=qC;��C<
=C<p�C<�
C==qC=��C=��C>Q�C>�RC?�C?�\C?�C@\)C@�RCA33CA��CB  CB\)CB��CC33CC��CD
=CDp�CD�
CEG�CE�CF{CFz�CF�CGQ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                            ?u@�\@=p�@�G�@��\@�G�@�p�A   A��A\)A*�HA?\)A_\)A\)A�  A�Q�A�  A�Q�AУ�A�\)A�B Q�Bz�B  B�
B�
B(  B0(�B8(�B@  BH  BPQ�BXQ�B`  Bh  Bp  Bw�
B�  B�{B�(�B�  B��B�{B�  B�  B�(�B�  B��B�  B�{B�  B��B�{B��B�{B�(�B�{B�  B�  B�{B�  B�(�B�(�B�(�B�(�B�{B�(�B�(�B�{C   C{C{C  C  C

=C{C{C  C
=C
=C
=C  C  C��C��C��C!��C#��C%�C(  C*{C,{C.
=C0{C1��C3��C5��C7�HC9��C<{C=��C?�CA��CD  CF
=CH{CJ�CL{CM�CP  CR{CS��CV{CX{CZ
=C\
=C^
=C`�Cb
=Cc��Ce��Ch  Cj  Ck��Cn  Cp{Cr  Ct  Cv  Cx  Cz
=C|  C~  C�C�  C�C�  C�C�  C���C���C���C�C�
=C�C�  C���C�  C�C�C�C�  C�  C�  C�C�
=C�  C���C�  C�C�
=C�  C���C�  C�C�C�C�C���C���C�  C�  C�C�
=C�C�
=C�C�
=C�  C���C���C���C�  C�  C�C�
=C�
=C���C���C�  C�  C�  C�  C�C�C�
=C�
=C�  C�C�C�  C�  C���C�C�\C�C���C�  C�  C�  C���C�C�  C���C���C���C�  C���C���C�
=C�C�  C�
=C�\C�  C��C��C���C���C���C�  C�  C�  C���C���C�  C���C���C���C���C���C�  C�  C�  C�C���C���C���C�C�
=C�  C�C�C�  C���C���C���C���C�  C�
=C�D �D � D�Dz�D��D��D�D}qD��D}qD��Dz�D�qD��DD��D�D��D	  D	z�D	�qD
}qD
��D}qD  D� D�qDz�D�qD�DD}qD  D}qD  D}qD  D��D�D� D�qDz�D�qD}qD��Dz�D��D� D  D� D�D��DD� D��D� D�D� D�qD��D  D}qD  D� D�qD � D!  D!}qD"�D"��D#  D#� D$  D$��D%�D%� D%�qD&��D'�D'� D(  D(� D(�qD)}qD)�qD*}qD+  D+� D,�D,��D-�D-��D.�D.}qD.�RD/z�D0  D0��D1  D1z�D2  D2��D2�qD3}qD3�qD4� D5  D5�D6
=D6��D6�RD7xRD7�qD8� D9D9��D:D:��D;  D;� D;�qD<z�D<��D=}qD>�D>� D>��D?xRD?��D@}qD@��DA}qDB  DB� DC  DC��DD�DD}qDD�qDE}qDE�qDF�DG�DG�DH�DH��DI�DI��DJDJ��DK  DK��DL  DL��DMDM��DN�DN� DO�DO� DO��DP� DQ�DQ� DR  DR��DS�DS}qDS��DT� DU�DU�DV  DV}qDW�DW�DX�DXz�DX�qDY� DY�qDZ}qD[�D[��D\  D\� D\�qD]}qD]�qD^��D_�D_��D`�D`��Da�Da��Db  Db��Dc  Dcz�Dc�qDd� De  De}qDe��Df}qDg  Dg��Dh  Dh}qDi�Di�Dj  Dj}qDj��Dk}qDk�qDl� Dm�Dm� Dm�qDn}qDo  Do}qDo�qDp��Dq  DqxRDq��Dr��Ds  Ds}qDt�Dt� Du  Du�DvDv}qDv�qDw� Dw�qDx� Dy  Dy� Dy�qDz� D{D{�D{�qD|� D}  D}}qD~�D~��D  D}qD��D�>�D�� D�� D���D�>�D�� D�� D�  D�B�D�� D���D���D�>�D�� D��HD�  D�@ D�� D�� D�  D�=qD�~�D��qD���D�AHD��HD���D���D�AHD�� D�� D���D�>�D��HD�D�  D�@ D�~�D���D�  D�AHD�~�D���D���D�AHD�� D���D�HD�>�D�� D���D��qD�>�D�� D��HD�  D�>�D�~�D�� D�  D�AHD��HD��HD�  D�>�D�� D���D���D�>�D�~�D���D���D�>�D�}qD�� D��D�AHD��HD��qD�  D�B�D�~�D�� D��D�B�D��HD�� D�HD�AHD�� D��HD�  D�>�D�� D���D�  D�AHD��HD�� D�HD�@ D�~�D��HD��D�AHD�� D���D�HD�AHD��HD�� D�  D�AHD��HD�� D��qD�>�D�~�D��qD�  D�AHD�� D�� D�HD�AHD�}qD���D�  D�@ D�� D�� D�HD�AHD�~�D���D�HD�@ D�~�D��qD���D�AHD�� D���D���D�@ D�� D���D�HD�AHD�� D��qD��qD�@ D���D�D�  D�@ D���D��HD�HD�C�D���D���D��qD�@ D�~�D��qD���D�AHD�~�D���D�HD�AHD�~�D��)D��)D�<)D�� D���D��D�ED���D�� D�  D�AHD�~�D��qD���D�AHD���D��HD���D�>�D�}qD�� D�HD�B�D��HD�� D�  D�B�D���D�� D��D�AHD�}qD��)D���D�=qD�}qD�� D��D�B�D�~�D���D���D�=qD��HD�� D���D�B�D��HD��HD�HD�=qD�}qD�� D��D�B�D�~�D���D��D�B�D��HD�D���D�>�D��HD��HD��qD�=qD�� D��HD��qD�=qD�� D�� D�  D�AHD�~�D��qD��qD�>�D��HD���D�  D�B�D���D�� D��qD�>�D�}qD��HD��D�@ D�~�D¾�D�  D�@ DÀ D�� D�HD�AHDĀ D�D��D�@ Dŀ D��HD�HD�@ D�~�D�� D�HD�>�D�~�D��HD�HD�>�DȁHD��HD�  D�@ D�~�Dɾ�D���D�@ D�~�Dʾ�D�  D�@ Dˀ D˾�D�HD�@ D�}qD̾�D���D�@ D̀ D;�D�HD�>�D�}qD�� D�  D�=qDπ D�� D���D�AHDЀ Dо�D�HD�@ Dр D�D�HD�@ DҀ DҾ�D�  D�B�DӁHD�� D�  D�>�D�~�D�� D�  D�>�DՁHD��HD�HD�@ D�~�D��HD�  D�@ D�~�D�� D�HD�AHD؁HD��HD�  D�>�DفHD�� D���D�@ D�~�D�� D�HD�@ DہHD�� D��qD�>�D܀ D�D��D�AHD݀ D�� D�  D�@ Dހ D�� D�  D�AHD߀ D�� D��D�@ D�~�DྸD��qD�@ D�HD�� D�  D�>�D�~�D��HD�HD�@ D� D㾸D�HD�AHD�HD�� D���D�=qD�}qD�qD��qD�=qD�~�D澸D�  D�AHD�HD��HD�  D�>�D�}qD�qD��qD�>�D�}qD龸D�HD�@ D�}qD�� D�HD�>�D� D뾸D���D�@ D�HD�� D�  D�@ D�}qD���D���D�=qD�HD��HD�HD�AHD� D�D���D�>�D�~�D�D���D�>�D� D�D��)D�@ D� D�� D�HD�AHD�HD�� D�HD�AHD� D��qD���D�@ D�� D���D�  D�@ D�~�D��qD���D�>�D��HD�D�HD�@ D�� D�� D�  D�@ D�~�D���D��qD�=qD�|)>#�
?\)?W
=?���?���?��H@(�@0��@J=q@k�@}p�@�\)@��R@�ff@�Q�@\@�33@��H@�=q@��HA�A
�HA�AffA�RA#33A*�HA2�\A7
=A?\)AC33AI��AQG�AU�A]p�AaG�Ai��Amp�AuA|(�A�  A�(�A�ffA�=qA�z�A�Q�A��A�A�G�A��A�\)A�G�A�p�A�\)A�=qA�A��A��A�A�G�A��A�
=A�G�A�p�AǮA��
A�ffAҏ\A�p�A�Q�A�z�A�
=A�33A�p�A�=qA��A�  A�z�A��RA��\A�{B ��B�RB�
B�B
=B��B
�\B�Bp�B=qB  B��B�\B\)Bz�BB�\B  B��BffB�HB(�Bp�B=qB�
B z�B!�B"�RB$(�B$��B%B'\)B(  B)��B*�\B+33B,��B-p�B/
=B/�B1G�B2{B3
=B4z�B5�B6�HB7�B8z�B:{B:�\B;�B=G�B=�B?�B@z�BAG�BB�HBC�
BDz�BF{BF�RBG�
BIG�BJ{BK33BL��BMG�BNffBO�
BPz�BR{BS33BS�
BUp�BU�BW
=BX��BYG�BZ=qB[�
B\Q�B]��B_
=B_�Ba�Ba��Bc33Bd(�Bd��BfffBf�HBh(�Bip�Bj{Bk�Bl(�Bm��Bn{Bo33Bp��BqG�BrffBs�BtQ�BuBvffBw\)Bx��ByG�Bz�RB{�
B|z�B~{B~�HB�B��\B��HB�G�B�  B�=qB��RB��B�B�z�B���B�G�B��B�(�B��HB�\)B���B�Q�B��\B�
=B��B�  B��\B��B�\)B��B�z�B��RB�G�B��
B�  B���B���B�p�B�  B�=qB���B�G�B���B�=qB�ffB��HB���B��
B�ffB�
=B�G�B��B�z�B��RB�\)B��B�(�B���B�\)B���B�{B��HB�G�B���B�Q�B���B�
=B��
B�Q�B��RB�p�B�  B�=qB�
=B���B��B��RB��B���B�Q�B��HB�33B�  B��\B��HB��B�=qB���B�p�B��
B�Q�B�33B��B�  B���B�33B��B��\B��HB�p�B�=qB���B��B��B�Q�B��RB��B��B��\B�G�B���B�Q�B�
=B�\)B�  B���B�\)B�B��\B�33B��B�(�B��HB��B��
B�ffB�G�B�B�{B��RB��B�{B�z�B��B��
B�ffB���Bř�B��BƏ\B�p�B�  B�Q�B���B�B�=qBʸRB�p�B�(�B̸RB�33B�  BΏ\B��HBϙ�B�ffB���B�\)B�(�BҸRB�G�BӮB�Q�B�33B�B�(�B���Bי�B�(�Bأ�B�33B�  Bڣ�B�
=Bۙ�B�z�B�
=B�\)B�{B���B߅B��B�z�B�33B��B�Q�B��HB�B�Q�B��HB�G�B��B���B�\)B�B�ffB�33B��
B�z�B���B뙚B�=qB��B��B�Q�B�RB�\)B�=qB��HB�B��
B��B�33B�(�B���B�33B��B��\B�\)B�(�B���B��B��B��RB�\)B�=qB���B���B�{B���B�\)C 
=C ffC ��C33C�CC�Cp�C�
CG�C��C��C(�C�C�CG�C�RC
=CffC�RC  CQ�C�RC
=C�C�
C	33C	z�C	C
(�C
z�C
��CQ�C�C��CG�C�C  C�C�HC=qC��C�HC33C�\C��Cp�C��C(�C�C�C33C�C�HCG�C��C{Cz�C�
C33C�C�
C(�C�C�HC=qC�\C��CQ�CC(�C�C�HC=qC��C��CG�C��C��C\)CC�C�\C  CffC��C33C�C�C Q�C �C!  C!Q�C!�RC"{C"z�C"�
C#G�C#C$�C$�C$�HC%33C%��C&  C&\)C&C'�C'��C(
=C(p�C(��C)33C)�\C)�HC*33C*��C+  C+\)C+�RC,�C,�\C-  C-ffC-�RC.�C.�C.�
C/33C/�C/�C0=qC0�C1
=C1p�C1�
C2=qC2��C3  C3p�C3�HC433C4��C4��C5\)C5�RC6
=C6p�C6C7(�C7�\C7��C8Q�C8�RC9�C9�C9��C:ffC:�
C;=qC;��C<
=C<p�C<�
C==qC=��C=��C>Q�C>�RC?�C?�\C?�C@\)C@�RCA33CA��CB  CB\)CB��CC33CC��CD
=CDp�CD�
CEG�CE�CF{CFz�CF�CGQ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bNA�ffA�dZA�ffA�dZA�dZA�bNA�dZA�bNA�dZA�bNA�bNA�^5A�Q�A�O�A�Q�A�`BA�^5A�I�A�VAҼjAҶFAҧ�Aҥ�Aҟ�Aҟ�Aҟ�Aҡ�Aҡ�Aҟ�Aҝ�Aқ�Aҙ�Aҕ�AғuAғuAғuAґhAҏ\Aҏ\A҇+A�&�A���A��A�M�A�hsA�E�AʾwA�XA�K�A�XA�jA��AƟ�A�ƨAş�A�bA��A���A�ZA�S�A���A�
=A�v�A��mA�(�A�{A�A�$�A���A��^A��PA�|�A���A��\A�XA�ZA���A��FA�\)A��A�dZA�ƨA�~�A�ƨA�x�A��A�XA�oA�;dA�|�A��A��;A��;A�r�A���A�VA���A��;A��7A�A��TA��A�?}A��mA�?}A���A��A� �A���A�&�A�ffA��wA��DA��9A�O�A��9A�bNA?}AxbNAr�Ap��AoAi�7AeC�A`�A^��A[x�AX�HAW�AT��AR�\AOAM%AK�7AH�uAH$�AD�ACdZAA\)A@-A>ZA9��A6$�A5VA4�9A2��A0��A0A�A-��A+��A*ffA)t�A'��A'K�A&ĜA%��A$��A$jA#�#A#?}A"��A!�-A bNAl�A��A�AAbNA��AJA��A�PA�AȴA��A�RAƨAA�PAn�A;dA��A�A9XAK�A�mA7LA~�A�PA
5?A	�A	oA��A��AVA�7AoA�AA�A"�Az�AdZAVA�A�A��A�RA�PA&�A �A ��A �+A z�@���@��!@��^@��@��`@�V@�r�@���@�@�hs@���@��@���@���@�t�@��m@��j@�?}@���@�|�@�|�@��+@��@�p�@���@���@�@�@�~�@�=q@��@�j@�1'@�S�@��@�^5@���@��@���@�^@�X@�O�@��@�@��@��@�p�@�D@�w@��@�7@�V@���@��`@� �@��@�ff@�@�O�@ܓu@�9X@��;@�33@�n�@ج@��;@֟�@ՙ�@�@��@�X@�&�@ԛ�@�=q@�K�@��@�&�@��@�9X@˥�@�@�n�@�@�p�@��@ʇ+@ɲ-@�E�@�dZ@�ƨ@˅@˝�@��@ʗ�@�n�@�V@�$�@ɡ�@�p�@�&�@�j@Ǖ�@�+@���@�V@�@�x�@���@���@ēu@�j@�(�@�K�@�v�@��@¸R@�5?@��-@�@�?}@��w@���@�@�/@���@���@��@�O�@�5?@��@��
@�K�@�ƨ@��w@��y@��\@�5?@�ȴ@�@��+@�p�@�Ĝ@���@��w@���@�@���@�5?@��-@�V@��u@�A�@��m@��@��@��R@��\@�v�@�@���@���@��T@���@���@��h@�hs@�`B@�G�@�7L@�7L@���@�Q�@��m@��P@�K�@���@�$�@��T@��^@��7@�G�@�bN@�b@��@��;@��y@��\@�$�@�hs@�V@�Ĝ@�Z@�  @�ƨ@�|�@�\)@���@�n�@�$�@��#@���@�X@��@��@�Ĝ@��D@�j@��@���@�dZ@�o@�M�@��T@���@��7@�x�@�&�@��@�r�@�A�@�b@��;@�ƨ@��@���@�t�@�
=@���@�M�@��@�@��#@��^@�`B@��`@��u@�r�@�  @��
@���@�K�@��H@���@�M�@�{@��-@�G�@��`@���@���@�bN@�Q�@�1@��
@���@�S�@�+@�@��@�ȴ@�V@��@�hs@���@��@���@��u@��D@�j@��m@��@���@���@���@��\@�-@�@���@��@���@��h@�/@��j@��u@�j@�1'@��;@��@���@��@�l�@�33@���@���@��!@���@�n�@�=q@��@��-@�x�@�?}@���@��@�r�@�I�@�1'@��@�1@���@��m@��@�33@�"�@��@���@���@�^5@��@���@��h@�hs@�`B@�hs@�O�@�G�@�/@�%@��@���@���@�r�@�1@���@��@�C�@���@�v�@�=q@���@�@�x�@�hs@�`B@��/@���@��@�I�@�b@��@��;@���@���@�ƨ@��F@��@���@�S�@�@��H@�v�@�{@�@�@��@��^@�hs@���@��/@���@�bN@�Z@�I�@�@|�@+@~�@~�+@~V@}�@|I�@{�m@{ƨ@{��@{S�@{33@z~�@y7L@y%@x�9@xA�@w�@wK�@w+@vV@u@u/@t��@tZ@s��@s�@so@rn�@rM�@q��@qhs@p�9@pb@o�@o|�@ol�@o\)@oK�@o;d@nȴ@nff@m�-@l�D@k��@k�F@k33@j��@jM�@i��@iX@h�@g�@gl�@fȴ@f��@fff@e�@eO�@e�@d�j@dz�@d9X@d�@c��@cS�@b��@bM�@a�@a��@aG�@`��@`A�@_�;@_|�@^�y@^�@^�R@^�+@^v�@^E�@^$�@]�@]�-@]�h@]?}@]V@\�@\Z@[��@Z�H@Z�!@Zn�@Z-@Y��@YG�@Y�@Y%@X�`@X��@X�@XQ�@X  @W�P@W|�@Wl�@W�@V�@V��@V5?@U�-@U`B@T�j@T�@T�D@Tj@S�m@SS�@So@R�@R�H@Rn�@Q�@Q��@QG�@P�`@P�@Pb@O��@O�@O�P@OK�@O
=@Nȴ@N�R@N�+@N@M@Mp�@L�D@K�
@K�@Kt�@KS�@KC�@K33@K@J��@J�@I��@I&�@H�@G��@G��@Gl�@Fȴ@F{@E@E�-@E��@EO�@E/@E/@D��@D�/@D�j@D��@Dj@D(�@C��@Cƨ@CdZ@C@B�H@B�\@B=q@B�@A�^@AX@@��@@Ĝ@@��@@Q�@@b@?l�@>�@>��@>�+@>ff@>V@>5?@>$�@=@=�@=�@=V@<�j@<j@<9X@;�
@;�@;"�@:��@:�\@:-@9�@9��@97L@8��@8Q�@81'@8b@8  @7�;@7��@7|�@7
=@6�R@6v�@6ff@6E�@6{@5�@5��@5�h@5O�@5V@5V@4�/@4�@4j@49X@3�
@2�@2��@2�\@2n�@2J@1X@1&�@0��@0��@0�@0A�@0  @/��@/�P@/l�@/K�@/+@/�@/�@.��@.ȴ@.��@.V@-�-@-�@,��@,9X@,1@+��@+�m@+�m@+ƨ@+��@+o@+@*�H@*�!@*~�@*=q@*J@*J@)�@)G�@(�u@(Q�@(1'@(b@'�@'�;@'�w@'l�@'
=@&ȴ@&E�@%��@%�h@%`B@%/@$�/@$�D@$z�@$I�@$9X@$(�@$�@#��@#ƨ@#S�@"��@"�\@"~�@"M�@!�@!�#@!��@!x�@!G�@!%@ Ĝ@ r�@ b@�w@�P@;d@
=@�@��@ff@�@��@�h@�@�@`B@?}@/@�/@z�@9X@1@�m@��@��@dZ@33@o@�@��@M�@�@��@��@��@hs@G�@%@��@�9@��@�u@Q�@�;@��@;d@�@V@$�@@�T@��@�-@�@/@V@�@�j@�@z�@�m@ƨ@��@�@S�@33@"�@�H@�!@M�@-@��@��@hs@�@��@�u@r�@A�@  @�w@�w@�P@\)@+@��@�R@v�@V@$�@@�h@�@�@�j@j@1@ƨ@��@t�@dZ@C�@o@@
�@
�H@
��@
�!@
��A�E�A�O�A�hsA�dZA�ffA�hsA�bNA�ffA�dZA�bNA�jA�ffA�ffA�dZA�bNA�ffA�`BA�hsA�`BA�bNA�ffA�bNA�bNA�dZA�`BA�hsA�bNA�dZA�ffA�`BA�bNA�`BA�`BA�bNA�^5A�dZA�\)A�Q�A�Q�A�ZA�M�A�O�A�M�A�I�A�XA�Q�A�Q�A�K�A�E�A�E�A�ZA�XA�S�A�XA�ZA�^5A�dZA�bNA�`BA�hsA�dZA�XA�XA�XA�ffA�hsA�O�A�5?A�;dA�+A�1'A�(�A���A�ƨA�AҺ^AҼjAҶFAҺ^AҼjA���AҲ-AҰ!Aҩ�Aҧ�AҬAҥ�Aҧ�Aҥ�Aҥ�Aҧ�Aң�Aҧ�Aҟ�Aң�Aҝ�Aҡ�Aҟ�Aҝ�Aҡ�Aҝ�Aҡ�Aҝ�Aҡ�Aҡ�Aҝ�Aң�Aҝ�Aҡ�Aҝ�Aҡ�Aҟ�Aҟ�Aҡ�Aҝ�Aҡ�Aҟ�Aҟ�Aң�Aҟ�Aң�Aҝ�Aң�Aҡ�Aҡ�Aҥ�Aҟ�Aң�Aң�Aҝ�Aң�Aҡ�Aҟ�Aң�Aҝ�Aҟ�Aҟ�Aқ�Aҟ�Aҟ�Aқ�Aҟ�Aҝ�Aқ�Aҟ�Aҝ�Aқ�Aҝ�Aқ�Aқ�Aҝ�Aҙ�Aқ�Aҝ�Aҙ�Aҝ�Aҙ�Aҗ�Aқ�Aҙ�Aҗ�Aқ�Aҕ�AғuAҕ�AғuAҗ�AғuAғuAҕ�AґhAҕ�AґhAғuAҕ�AґhAҕ�AґhAҕ�AґhAґhAҕ�AґhAғuAғuAґhAҕ�AґhAҏ\Aҕ�AґhAғuAғuAҏ\AғuAғuAҏ\AғuAғuAҍPAғuAҏ\AҍPAґhAҋDAҏ\AҍPAҍPAґhAҋDAҏ\Aҏ\AҍPAғuAҏ\Aҏ\AғuAҍPAҍPAҏ\A҉7A҉7A҃A�z�A�x�A�v�A�hsA�dZA�VA�S�A�M�A�VAѲ-A�v�A�VA��A�ĜAЍPA�?}A�v�Aΰ!A�hsA�VA�33A���A��A��A��A��TA��/A���A���A�ĜA͟�A͇+A�|�A�A�A��/A�E�A���A˥�A˛�AˍPAˁA�ffA��Aʏ\A�C�A��A�1A�oA�n�AʓuAʕ�AʬA�A�ĜA�ȴA���A�ƨA�Aʴ9Aʙ�AʃA�l�A�I�A���Aɏ\A�n�A�p�A�\)A�M�A�C�A�5?A��A���Aȥ�AȃA�^5A��A��A��HAǲ-AǃA�jA�XA�G�A�?}A�=qA�5?A�1'A�+A� �A�A���A���A��mA���A���Aư!A�r�A�&�A��A��A�ƨAŴ9AŶFAŰ!AŮAŰ!AŬAť�Ať�Aţ�Ař�Aŉ7A�|�A�l�A�33A��A���A�A�x�A�p�A�XA�G�A�(�A�ƨAüjAöFAøRAú^AþwA�A��A��A��HA�ȴAÑhA�VA�$�A��A���A�ƨA�ȴA§�A�p�A�
=A�r�A���A��A�r�A�"�A��A��\A��A�7LA�ĜA�t�A�&�A��wA�O�A��\A�{A�ȴA�jA�;dA��A�1A��HA��!A��A��A�O�A�7LA�7LA�=qA�;dA�9XA�9XA�(�A���A���A�O�A��A�ĜA��-A�|�A���A�K�A��`A�p�A�A��`A��9A�jA��A��yA���A�XA�G�A�1'A���A���A���A�XA�(�A��A���A�`BA�C�A�1'A�A��FA�t�A�E�A��hA��TA�A���A�Q�A��TA��A�/A��FA�XA�&�A�1A��A��mA�ȴA��A�O�A��+A��HA�n�A�=qA�$�A�oA��yA��PA�hsA�=qA�1'A�9XA�33A��A��uA�G�A�%A��TA���A��jA���A���A���A��uA�~�A�\)A�9XA�$�A��A��A�VA��`A�ĜA���A��A�v�A�`BA�S�A�I�A� �A��HA�&�A�$�A�ĜA���A���A��A�z�A�v�A�hsA�C�A�
=A���A��A�G�A��`A���A�jA�ZA�G�A�"�A��A�%A��`A��A��FA��A�M�A�A�A�I�A�7LA��A��A��A�oA��A��#A��A�oA��HA���A��!A���A��+A�p�A�bNA�XA�Q�A�A�A��A���A���A�z�A���A�ƨA��uA�r�A�I�A��A��/A���A���A���A��\A�|�A�jA�O�A�
=A��HA���A�|�A�r�A�hsA�`BA�&�A��yA��mA�ĜA���A�bNA���A�n�A��A�XA��A�ĜA���A��hA�JA�ĜA��hA�XA�33A�A�-A�|�A�;dA��A�%A��HA��HA��
A���A��9A���A���A�p�A��A��FA�~�A�I�A��yA��7A�?}A�A��
A���A��PA�z�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                            A�bNA�ffA�dZA�ffA�dZA�dZA�bNA�dZA�bNA�dZA�bNA�bNA�^5A�Q�A�O�A�Q�A�`BA�^5A�I�A�VAҼjAҶFAҧ�Aҥ�Aҟ�Aҟ�Aҟ�Aҡ�Aҡ�Aҟ�Aҝ�Aқ�Aҙ�Aҕ�AғuAғuAғuAґhAҏ\Aҏ\A҇+A�&�A���A��A�M�A�hsA�E�AʾwA�XA�K�A�XA�jA��AƟ�A�ƨAş�A�bA��A���A�ZA�S�A���A�
=A�v�A��mA�(�A�{A�A�$�A���A��^A��PA�|�A���A��\A�XA�ZA���A��FA�\)A��A�dZA�ƨA�~�A�ƨA�x�A��A�XA�oA�;dA�|�A��A��;A��;A�r�A���A�VA���A��;A��7A�A��TA��A�?}A��mA�?}A���A��A� �A���A�&�A�ffA��wA��DA��9A�O�A��9A�bNA?}AxbNAr�Ap��AoAi�7AeC�A`�A^��A[x�AX�HAW�AT��AR�\AOAM%AK�7AH�uAH$�AD�ACdZAA\)A@-A>ZA9��A6$�A5VA4�9A2��A0��A0A�A-��A+��A*ffA)t�A'��A'K�A&ĜA%��A$��A$jA#�#A#?}A"��A!�-A bNAl�A��A�AAbNA��AJA��A�PA�AȴA��A�RAƨAA�PAn�A;dA��A�A9XAK�A�mA7LA~�A�PA
5?A	�A	oA��A��AVA�7AoA�AA�A"�Az�AdZAVA�A�A��A�RA�PA&�A �A ��A �+A z�@���@��!@��^@��@��`@�V@�r�@���@�@�hs@���@��@���@���@�t�@��m@��j@�?}@���@�|�@�|�@��+@��@�p�@���@���@�@�@�~�@�=q@��@�j@�1'@�S�@��@�^5@���@��@���@�^@�X@�O�@��@�@��@��@�p�@�D@�w@��@�7@�V@���@��`@� �@��@�ff@�@�O�@ܓu@�9X@��;@�33@�n�@ج@��;@֟�@ՙ�@�@��@�X@�&�@ԛ�@�=q@�K�@��@�&�@��@�9X@˥�@�@�n�@�@�p�@��@ʇ+@ɲ-@�E�@�dZ@�ƨ@˅@˝�@��@ʗ�@�n�@�V@�$�@ɡ�@�p�@�&�@�j@Ǖ�@�+@���@�V@�@�x�@���@���@ēu@�j@�(�@�K�@�v�@��@¸R@�5?@��-@�@�?}@��w@���@�@�/@���@���@��@�O�@�5?@��@��
@�K�@�ƨ@��w@��y@��\@�5?@�ȴ@�@��+@�p�@�Ĝ@���@��w@���@�@���@�5?@��-@�V@��u@�A�@��m@��@��@��R@��\@�v�@�@���@���@��T@���@���@��h@�hs@�`B@�G�@�7L@�7L@���@�Q�@��m@��P@�K�@���@�$�@��T@��^@��7@�G�@�bN@�b@��@��;@��y@��\@�$�@�hs@�V@�Ĝ@�Z@�  @�ƨ@�|�@�\)@���@�n�@�$�@��#@���@�X@��@��@�Ĝ@��D@�j@��@���@�dZ@�o@�M�@��T@���@��7@�x�@�&�@��@�r�@�A�@�b@��;@�ƨ@��@���@�t�@�
=@���@�M�@��@�@��#@��^@�`B@��`@��u@�r�@�  @��
@���@�K�@��H@���@�M�@�{@��-@�G�@��`@���@���@�bN@�Q�@�1@��
@���@�S�@�+@�@��@�ȴ@�V@��@�hs@���@��@���@��u@��D@�j@��m@��@���@���@���@��\@�-@�@���@��@���@��h@�/@��j@��u@�j@�1'@��;@��@���@��@�l�@�33@���@���@��!@���@�n�@�=q@��@��-@�x�@�?}@���@��@�r�@�I�@�1'@��@�1@���@��m@��@�33@�"�@��@���@���@�^5@��@���@��h@�hs@�`B@�hs@�O�@�G�@�/@�%@��@���@���@�r�@�1@���@��@�C�@���@�v�@�=q@���@�@�x�@�hs@�`B@��/@���@��@�I�@�b@��@��;@���@���@�ƨ@��F@��@���@�S�@�@��H@�v�@�{@�@�@��@��^@�hs@���@��/@���@�bN@�Z@�I�@�@|�@+@~�@~�+@~V@}�@|I�@{�m@{ƨ@{��@{S�@{33@z~�@y7L@y%@x�9@xA�@w�@wK�@w+@vV@u@u/@t��@tZ@s��@s�@so@rn�@rM�@q��@qhs@p�9@pb@o�@o|�@ol�@o\)@oK�@o;d@nȴ@nff@m�-@l�D@k��@k�F@k33@j��@jM�@i��@iX@h�@g�@gl�@fȴ@f��@fff@e�@eO�@e�@d�j@dz�@d9X@d�@c��@cS�@b��@bM�@a�@a��@aG�@`��@`A�@_�;@_|�@^�y@^�@^�R@^�+@^v�@^E�@^$�@]�@]�-@]�h@]?}@]V@\�@\Z@[��@Z�H@Z�!@Zn�@Z-@Y��@YG�@Y�@Y%@X�`@X��@X�@XQ�@X  @W�P@W|�@Wl�@W�@V�@V��@V5?@U�-@U`B@T�j@T�@T�D@Tj@S�m@SS�@So@R�@R�H@Rn�@Q�@Q��@QG�@P�`@P�@Pb@O��@O�@O�P@OK�@O
=@Nȴ@N�R@N�+@N@M@Mp�@L�D@K�
@K�@Kt�@KS�@KC�@K33@K@J��@J�@I��@I&�@H�@G��@G��@Gl�@Fȴ@F{@E@E�-@E��@EO�@E/@E/@D��@D�/@D�j@D��@Dj@D(�@C��@Cƨ@CdZ@C@B�H@B�\@B=q@B�@A�^@AX@@��@@Ĝ@@��@@Q�@@b@?l�@>�@>��@>�+@>ff@>V@>5?@>$�@=@=�@=�@=V@<�j@<j@<9X@;�
@;�@;"�@:��@:�\@:-@9�@9��@97L@8��@8Q�@81'@8b@8  @7�;@7��@7|�@7
=@6�R@6v�@6ff@6E�@6{@5�@5��@5�h@5O�@5V@5V@4�/@4�@4j@49X@3�
@2�@2��@2�\@2n�@2J@1X@1&�@0��@0��@0�@0A�@0  @/��@/�P@/l�@/K�@/+@/�@/�@.��@.ȴ@.��@.V@-�-@-�@,��@,9X@,1@+��@+�m@+�m@+ƨ@+��@+o@+@*�H@*�!@*~�@*=q@*J@*J@)�@)G�@(�u@(Q�@(1'@(b@'�@'�;@'�w@'l�@'
=@&ȴ@&E�@%��@%�h@%`B@%/@$�/@$�D@$z�@$I�@$9X@$(�@$�@#��@#ƨ@#S�@"��@"�\@"~�@"M�@!�@!�#@!��@!x�@!G�@!%@ Ĝ@ r�@ b@�w@�P@;d@
=@�@��@ff@�@��@�h@�@�@`B@?}@/@�/@z�@9X@1@�m@��@��@dZ@33@o@�@��@M�@�@��@��@��@hs@G�@%@��@�9@��@�u@Q�@�;@��@;d@�@V@$�@@�T@��@�-@�@/@V@�@�j@�@z�@�m@ƨ@��@�@S�@33@"�@�H@�!@M�@-@��@��@hs@�@��@�u@r�@A�@  @�w@�w@�P@\)@+@��@�R@v�@V@$�@@�h@�@�@�j@j@1@ƨ@��@t�@dZ@C�@o@@
�@
�H@
��@
�!@
��A�E�A�O�A�hsA�dZA�ffA�hsA�bNA�ffA�dZA�bNA�jA�ffA�ffA�dZA�bNA�ffA�`BA�hsA�`BA�bNA�ffA�bNA�bNA�dZA�`BA�hsA�bNA�dZA�ffA�`BA�bNA�`BA�`BA�bNA�^5A�dZA�\)A�Q�A�Q�A�ZA�M�A�O�A�M�A�I�A�XA�Q�A�Q�A�K�A�E�A�E�A�ZA�XA�S�A�XA�ZA�^5A�dZA�bNA�`BA�hsA�dZA�XA�XA�XA�ffA�hsA�O�A�5?A�;dA�+A�1'A�(�A���A�ƨA�AҺ^AҼjAҶFAҺ^AҼjA���AҲ-AҰ!Aҩ�Aҧ�AҬAҥ�Aҧ�Aҥ�Aҥ�Aҧ�Aң�Aҧ�Aҟ�Aң�Aҝ�Aҡ�Aҟ�Aҝ�Aҡ�Aҝ�Aҡ�Aҝ�Aҡ�Aҡ�Aҝ�Aң�Aҝ�Aҡ�Aҝ�Aҡ�Aҟ�Aҟ�Aҡ�Aҝ�Aҡ�Aҟ�Aҟ�Aң�Aҟ�Aң�Aҝ�Aң�Aҡ�Aҡ�Aҥ�Aҟ�Aң�Aң�Aҝ�Aң�Aҡ�Aҟ�Aң�Aҝ�Aҟ�Aҟ�Aқ�Aҟ�Aҟ�Aқ�Aҟ�Aҝ�Aқ�Aҟ�Aҝ�Aқ�Aҝ�Aқ�Aқ�Aҝ�Aҙ�Aқ�Aҝ�Aҙ�Aҝ�Aҙ�Aҗ�Aқ�Aҙ�Aҗ�Aқ�Aҕ�AғuAҕ�AғuAҗ�AғuAғuAҕ�AґhAҕ�AґhAғuAҕ�AґhAҕ�AґhAҕ�AґhAґhAҕ�AґhAғuAғuAґhAҕ�AґhAҏ\Aҕ�AґhAғuAғuAҏ\AғuAғuAҏ\AғuAғuAҍPAғuAҏ\AҍPAґhAҋDAҏ\AҍPAҍPAґhAҋDAҏ\Aҏ\AҍPAғuAҏ\Aҏ\AғuAҍPAҍPAҏ\A҉7A҉7A҃A�z�A�x�A�v�A�hsA�dZA�VA�S�A�M�A�VAѲ-A�v�A�VA��A�ĜAЍPA�?}A�v�Aΰ!A�hsA�VA�33A���A��A��A��A��TA��/A���A���A�ĜA͟�A͇+A�|�A�A�A��/A�E�A���A˥�A˛�AˍPAˁA�ffA��Aʏ\A�C�A��A�1A�oA�n�AʓuAʕ�AʬA�A�ĜA�ȴA���A�ƨA�Aʴ9Aʙ�AʃA�l�A�I�A���Aɏ\A�n�A�p�A�\)A�M�A�C�A�5?A��A���Aȥ�AȃA�^5A��A��A��HAǲ-AǃA�jA�XA�G�A�?}A�=qA�5?A�1'A�+A� �A�A���A���A��mA���A���Aư!A�r�A�&�A��A��A�ƨAŴ9AŶFAŰ!AŮAŰ!AŬAť�Ať�Aţ�Ař�Aŉ7A�|�A�l�A�33A��A���A�A�x�A�p�A�XA�G�A�(�A�ƨAüjAöFAøRAú^AþwA�A��A��A��HA�ȴAÑhA�VA�$�A��A���A�ƨA�ȴA§�A�p�A�
=A�r�A���A��A�r�A�"�A��A��\A��A�7LA�ĜA�t�A�&�A��wA�O�A��\A�{A�ȴA�jA�;dA��A�1A��HA��!A��A��A�O�A�7LA�7LA�=qA�;dA�9XA�9XA�(�A���A���A�O�A��A�ĜA��-A�|�A���A�K�A��`A�p�A�A��`A��9A�jA��A��yA���A�XA�G�A�1'A���A���A���A�XA�(�A��A���A�`BA�C�A�1'A�A��FA�t�A�E�A��hA��TA�A���A�Q�A��TA��A�/A��FA�XA�&�A�1A��A��mA�ȴA��A�O�A��+A��HA�n�A�=qA�$�A�oA��yA��PA�hsA�=qA�1'A�9XA�33A��A��uA�G�A�%A��TA���A��jA���A���A���A��uA�~�A�\)A�9XA�$�A��A��A�VA��`A�ĜA���A��A�v�A�`BA�S�A�I�A� �A��HA�&�A�$�A�ĜA���A���A��A�z�A�v�A�hsA�C�A�
=A���A��A�G�A��`A���A�jA�ZA�G�A�"�A��A�%A��`A��A��FA��A�M�A�A�A�I�A�7LA��A��A��A�oA��A��#A��A�oA��HA���A��!A���A��+A�p�A�bNA�XA�Q�A�A�A��A���A���A�z�A���A�ƨA��uA�r�A�I�A��A��/A���A���A���A��\A�|�A�jA�O�A�
=A��HA���A�|�A�r�A�hsA�`BA�&�A��yA��mA�ĜA���A�bNA���A�n�A��A�XA��A�ĜA���A��hA�JA�ĜA��hA�XA�33A�A�-A�|�A�;dA��A�%A��HA��HA��
A���A��9A���A���A�p�A��A��FA�~�A�I�A��yA��7A�?}A�A��
A���A��PA�z�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
�kB
�kB
�7B
��B
�B
�7B
��B
��B
�7B
�B
��B
�1B
��B
��B
��B
�1B
��B
�MB
��B
�MB
��B
�B
��B
��B
��B
�SB
�B
�B
�SB
�SB
�B
��B
��B
��B
�MB
��B
�FB
�oB
��B
�+B
�FB
�6B
�zB
��B6�BZBP}BQNBR�BT�B`�Br�BtB��B��B�FB��B�&B�B�B��B�iB}�Bt�BrGBY�BPHBJ#BN�B_;BjBl�BS�BCaBS�BC�BDgBA�B<jBMjBg�Bi�B]�B[�B^�B]/BZ�BM�BGEB=B.B&�B#B=B�B
��B
��B
�;B
�EB
��B
��B
�hB
�SB
~�B
v�B
g�B
d�B
]�B
W?B
E9B
1'B
'B
�B
	�B
;B	��B	��B	��B	�@B	�VB	|�B	d�B	N�B	?HB	-CB	B	�B��B�B��B��B��B�fB��B�,B�jB�B�2B�<BϫB�tB��B��B�XBÖB�aB��B��B�<B�BB��B��B�-B��B��B�B��B��BƨB˒B��B��BƨBƨBɆB��B�}B�HB͟B�BBϫB�B�NB��B�QB��B�B�&B�KB�sB�mB�B�B�B��B��B��B�JB��B	 �B	B	�B	�B	 �B	�B	SB	�B	�B	 B	{B	�B	�B	�B	�B	(�B	+B	&�B	(�B	,�B	,�B	0�B	4B	2�B	7B	8B	8�B	<�B	;�B	7�B	5�B	<jB	E9B	J�B	U�B	b�B	o5B	{�B	|�B	�B	��B	��B	�JB	�B	��B	�B	��B	�B	�{B	�{B	��B	��B	�{B	�B	�B	�$B	�1B	��B	�1B	��B	�B	��B	�VB	��B	�B	��B	��B	��B	�4B	��B	�nB	��B	��B	�RB	��B	�LB	�XB	�*B	��B	�eB	�wB	��B	��B	��B	�}B	��B	��B	�B	�0B	��B	�UB	��B	��B	�3B	��B	��B	�B	�qB	�IB	�B	�VB	��B	�B	�~B	�xB	��B	��B	��B	�*B	�kB	�zB	��B	�BB	�B	��B	��B	ĜB	�tB	��B	�#B	��B	�^B	�)B	͟B	�B	��B	�BB	ΥB	ѷB	�B	�B	��B	�6B	��B	�dB	�6B	уB	��B	�&B	��B	ӏB	��B	� B	�}B	̘B	�dB	̘B	�B	�}B	�mB	�)B	�B	��B	��B	�5B	��B	�HB	�HB	�vB	� B	�mB	�B	�ZB	��B	�B	�B	�B	�8B	��B	�fB	�2B	�8B	�fB	��B	�B	�B	��B	��B	�2B	�B	�
B	�B	�B	��B	�;B	�B	�AB	�B	��B	��B	��B	�B	�B	��B	�fB	�8B	�rB	��B	�lB	��B	�B	�B	�(B	�VB	�"B	��B
B	��B	�cB
B
AB
�B
{B
B
�B
SB
%B
�B
fB
	�B
�B
�B
	�B
B
�B
�B
\B
�B
�B
bB
hB
hB
�B
B
�B
FB
B
�B
B
�B
�B
SB
�B
$B
YB
�B
�B
+B
1B
B
kB
�B
�B
�B
�B
=B
�B
�B
�B
qB
�B
qB
�B
IB
�B
�B
~B
�B
�B
�B
�B
�B
VB
�B
�B
�B
�B
 'B
�B
�B
�B
VB
!B
�B
VB
�B
�B
�B
�B
�B
�B
�B
�B
�B
 \B
 \B
!�B
"�B
#�B
$B
$B
$B
#�B
%FB
%�B
%�B
&LB
&�B
'RB
'�B
'�B
'�B
'�B
'�B
(XB
'�B
'�B
'�B
($B
'�B
(�B
(�B
)*B
)_B
)�B
*�B
*�B
*�B
+B
+6B
+6B
+B
+6B
,=B
,=B
,=B
,B
,=B
,B
+6B
+6B
+�B
,qB
,�B
,�B
-wB
-�B
.B
.IB
.IB
.IB
.IB
.}B
.�B
/�B
/OB
.�B
.�B
/OB
/B
/B
/�B
/�B
0UB
0�B
0�B
2�B
2�B
2�B
33B
2�B
2�B
2�B
2�B
2-B
2-B
1�B
1�B
1[B
1�B
1�B
1�B
2�B
33B
33B
2�B
2�B
33B
4B
5�B
6zB
6�B
6�B
6zB
6zB
7LB
7�B
7�B
7�B
7�B
7�B
9$B
9$B
9�B
9�B
9�B
:*B
9�B
9�B
:�B
:�B
;dB
;0B
;�B
<B
;�B
<�B
=qB
=�B
=�B
>B
>wB
>�B
?HB
?}B
?B
?HB
?}B
@OB
@�B
A B
A B
A B
@�B
A B
@�B
A B
A B
A�B
B�B
C-B
B�B
CaB
C�B
C�B
D3B
EmB
FtB
F�B
GB
G�B
GzB
GzB
HB
H�B
H�B
IRB
IB
I�B
IRB
IRB
I�B
J#B
J�B
J�B
J�B
K^B
K�B
K�B
L0B
L�B
L�B
L�B
MB
MB
MB
M6B
M6B
MjB
MjB
MjB
M�B
M�B
M�B
NB
N�B
OB
OB
OvB
OvB
OvB
P}B
P}B
PB
P}B
P�B
P�B
P}B
QB
QB
QB
QB
Q�B
Q�B
QNB
Q�B
Q�B
RTB
R�B
R�B
R�B
R�B
S[B
S�B
S�B
S�B
S�B
T,B
TaB
T�B
T�B
U2B
U�B
V9B
V9B
V9B
V9B
VmB
V�B
W
B
V�B
W
B
WsB
W?B
W�B
X�B
YB
YKB
YB
YB
YB
YB
YB
Y�B
ZQB
ZQB
Z�B
[�B
[�B
[�B
[�B
\�B
]/B
]�B
]dB
]�B
^B
]�B
]�B
^B
^B
^5B
^B
^5B
^�B
^jB
^�B
_B
_pB
_;B
_�B
_�B
`B
`BB
`�B
`�B
`�B
aB
aHB
aHB
bNB
b�B
b�B
b�B
c B
c B
c B
c B
cTB
c�B
c�B
c�B
c�B
d&B
d&B
dZB
d�B
d�B
e,B
e,B
e`B
e�B
e�B
e�B
ffB
f�B
f�B
f�B
f�B
gB
f�B
gB
g8B
g�B
g�B
g�B
gmB
g8B
gB
gB
gB
g8B
gmB
gmB
g�B
hsB
h�B
h�B
h�B
h�B
iB
iB
iB
h�B
iB
h�B
iB
h�B
h�B
iB
iB
iB
iDB
iB
iB
iDB
iDB
iDB
iB
iDB
iDB
iDB
i�B
jKB
j�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l�B
l�B
m)B
m)B
m]B
m]B
m�B
m]B
m)B
ncB
o5B
o�B
pB
pB
p;B
p;B
p;B
poB
p�B
qB
qvB
q�B
rB
rB
rGB
rGB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
tB
t�B
tTB
t�B
t�B
t�B
t�B
t�B
t�B
uZB
u�B
u�B
v`B
v`B
v`B
v�B
v�B
wfB
w2B
w�B
w�B
w�B
w�B
x8B
xlB
xlB
xlB
y	B
yrB
y�B
zB
y�B
y�B
y�B
y�B
y�B
zDB
zDB
zxB
z�B
z�B
z�B
{B
{�B
|B
|�B
|�B
|�B
|�B
|�B
}VB
}"B
}VB
}�B
}�B
}�B
~(B
~]B
~]B
~]B
~�B
.B
cB
cB
cB
cB
cB
�4B
�4B
�4B
�4B
��B
��B
��B
�B
�;B
��B
��B
��B
�AB
��B
��B
�B
�{B
��B
�B
��B
�MB
��B
��B
��B
��B
��B
��B
��B
��B
�%B
��B
��B
��B
�+B
�_B
��B
�1B
�fB
�fB
��B
��B
�B
�7B
�7B
�lB
�lB
��B
��B
�	B
��B
��B
�1B
�	B
��B
�eB
�qB
��B
�B
�	B
��B
�qB
�B
�7B
��B
��B
��B
�1B
�=B
�=B
�eB
��B
�	B
��B
�=B
�_B
��B
�1B
��B
��B
�1B
�7B
��B
�1B
��B
�_B
��B
��B
��B
��B
�7B
�	B
�+B
��B
�_B
�B
�1B
��B
��B
��B
��B
��B
�kB
�_B
�+B
��B
��B
��B
�+B
�_B
��B
�eB
�$B
�eB
��B
��B
��B
��B
��B
�{B
�{B
��B
��B
��B
��B
��B
�MB
��B
��B
�{B
�uB
��B
��B
��B
��B
�{B
�SB
�B
�B
��B
�B
��B
�{B
��B
�B
�MB
��B
�FB
��B
�FB
��B
��B
��B
��B
�B
��B
�{B
��B
��B
��B
��B
�B
��B
��B
��B
�MB
��B
�B
��B
��B
��B
��B
�MB
�SB
�SB
�MB
�$B
��B
��B
�$B
��B
�B
��B
�MB
�YB
�B
��B
��B
��B
�B
�$B
��B
��B
��B
�MB
�B
��B
��B
�B
��B
�MB
��B
��B
�B
�$B
��B
�SB
�YB
��B
��B
��B
�MB
�$B
�SB
��B
��B
�MB
��B
��B
��B
��B
�B
��B
�B
�B
��B
�B
��B
�B
��B
�SB
�{B
�SB
��B
��B
��B
�{B
��B
��B
��B
�B
�MB
��B
��B
�B
�{B
�B
��B
��B
��B
��B
�MB
��B
��B
��B
�B
�B
�MB
�uB
�B
�{B
��B
��B
�@B
�B
��B
��B
��B
�uB
�oB
��B
��B
��B
��B
� B
��B
��B
�"B
�VB
��B
�B
��B
��B
�B
� B
��B
� B
��B
�+B
�nB
�RB
��B
�B
��B
�B
�zB
�B
��B
��B
��B
�FB
�zB
�FB
��B
�@B
�nB
�qB
�OB
�0B
��B
�6B
�wB
��B
�9B
��B
�-B
�qB
��B
�B
�*B
�B
�B+BBIB(�B9$B:*B?}BO�BOBS&BXyBW?BXB^5Bb�B[�BS�BO�BQ�BQNBN<BN�BO�BQ�BU2BR BR BS�BS&BG�BP�BQ�BR�BT�BU2BT�BR�BS�BS[BS&BT,BXBV9BU�BW?BZ�B\�B_pBl�Bl�BjKBsBt�BtTBr�BtTBt�Br|BsBtTBs�BsBt�Bv�Bv`Bv�B~�B��B�SB��B�YB��B��B��B�B��B�FB��B��B�MB��B��B�XBӏBҽBѷB��BбB�BȀB�RB̘B��B��B��B��B��B��B�B�BںB��B� B�0B�B�B�CB��B��B�B��B�FB��B�VB��B��B��B~�B�B�%B~(B�B|B|�B|PB|PB|BzDB{B��B�BoiBs�BoiBoiBt�B��B��Bp�Bj�B^�B]/B\�B\)BY�BW�BW?BR�BOvBO�BRTBNBPHBP}BI�BL0BJXBGzBF�BE9BMBF�BOBJ�Bj�BZ�BYKBXyBf2Be,Bk�BdZBr�Bm�BiyBg�Bg�BgBhsBi�BtBx�Bd�B\)BR�BGEBB�BNBF?BA�BAUB=BL�BT�B\�BTaBS�BK^BFtBB�BA�BB�BC-BD�BGzBF�BC�BB�BB'B>BB>BA�BEmBA�BD3B>�B=B;�B9�B:^B:�B>BBbNBO�BT�BS�B`BBe,Bh
Bl�Bn/BrGBkQBi�BbNBd�Bi�Bc�B[�BXEBYB]�BXEB[WB[�BZ�B]/Ba�BbNB^BZ�Ba|B\�B[�BY�B`BB\�BZ�Bb�Bd�BW�BT,BQ�BO�BN<BL0BI�BH�BGEBI�BG�BF�BCaBK�BD�B7�B9�B1[B2aB.}B4�B)�B&�B&B%�B&�B%�B%zB(XB$�B,=B�B!B~B=B(XB�B+BoB@B�BqB@B:BDB�B
�B
��B
��B
��B
�B
�B
��B
��B
�B�B
ӏB
��B
ϫB
��B
�KB
�aB
��B
�HB
�-B
��B
�$B
��B
��B
�B
�zB
��B
��B
��B
��B
��B
�.B
��B
��B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                            B
�B
��B
��B
�wB
�wB
�CB
��B
�B
�CB
��B
��B
�CB
�B
��B
�=B
��B
��B
��B
�=B
��B
�YB
��B
�YB
��B
�$B
��B
��B
��B
�_B
�*B
�*B
�_B
�_B
�*B
��B
��B
��B
�YB
��B
�RB
�{B
��B
�7B
�RB
�BB
��B
�
B.�BR)BH�BIZBJ�BL�BX�Bj�Bl+Bx�B��B�RB��B�2B�)B�B��BxuBu�Bl�BjSBQ�BHTBB/BF�BWGBb"Be BK�B;mBK�B;�B<sB9�B4vBEvB_�Ba�BU�BS�BV�BU;BR�BE�B?QB5B& B�BBIB�B
�B
��B
�GB
�QB
��B
��B
�tB
}_B
v�B
o	B
_�B
\�B
U�B
OKB
=EB
)3B
*B
�B
�B	�GB	��B	��B	��B	�LB	�bB	t�B	\�B	F�B	7TB	%OB	'B	�B�B�B� B��B��B�rB��B�8B�vB�)B�>B�HBǷB��B��B��B�dB��B�mB��B��B�HB�NB��B��B�9B�B��B�#B��B��B��BÞB��B��B��B��B��B��BȉB�TBūB�NBǷB� B�ZB��B�]B��BݡB�2B�WB�B�yB�(B�B�B��B��B��B�VB�B��B�B��B��B��B��B�_B��B	�B		B	�B	
�B	�B	�B	B	 �B	#B	�B	!B	$�B	$�B	(�B	,B	+B	/#B	0)B	0�B	4�B	3�B	/�B	-�B	4vB	=EB	B�B	M�B	Z�B	gAB	s�B	t�B	}+B	B	�B	�VB	�B	��B	�*B	��B	�$B	��B	��B	��B	��B	��B	�B	�$B	�0B	�=B	��B	�=B	��B	�B	��B	�bB	��B	�'B	��B	��B	��B	�@B	��B	�zB	��B	��B	�^B	��B	�XB	�dB	�6B	��B	�qB	��B	��B	��B	��B	��B	��B	��B	�'B	�<B	��B	�aB	�B	��B	�?B	��B	��B	�B	�}B	�UB	�B	�bB	��B	�!B	��B	��B	��B	��B	��B	�6B	�wB	��B	��B	�NB	�&B	��B	��B	��B	��B	��B	�/B	��B	�jB	�5B	ūB	�B	��B	�NB	ƱB	��B	� B	�B	��B	�BB	��B	�pB	�BB	ɏB	��B	�2B	��B	˛B	�
B	�,B	ȉB	ĤB	�pB	ĤB	�B	ȉB	�yB	�5B	�B	�B	��B	�AB	��B	�TB	�TB	؂B	�,B	�yB	ާB	�fB	��B	�B	ٽB	߭B	�DB	�
B	�rB	�>B	�DB	�rB	�
B	ݡB	ާB	�
B	�
B	�>B	�B	�B	�B	��B	��B	�GB	�B	�MB	�B	�B	��B	��B	�B	�"B	�B	�rB	�DB	�~B	��B	�xB	�B	�"B	�B	�4B	�bB	�.B	��B	�B	��B	�oB	�B	�MB	��B	��B	�%B	��B	�_B	�1B	��B
 rB
�B
 �B
 �B
�B
!B
�B
�B
hB
B
B
nB
	tB
	tB
	�B
B
�B
RB
B
�B
B
�B
�B
_B
�B
0B
eB
�B
�B
7B
=B
B
wB
�B
�B
�B
�B
IB
�B
�B
�B
}B
�B
}B
�B
UB
�B
�B
�B
�B
�B
�B
�B
�B
bB
�B
�B
�B
�B
3B
�B
�B
�B
bB
-B
�B
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
hB
hB
�B
�B
�B
B
B
B
�B
RB
�B
�B
XB
�B
^B
�B
�B
�B
�B
�B
 dB
�B
�B
�B
 0B
�B
 �B
 �B
!6B
!kB
"B
"�B
"�B
"�B
#B
#BB
#BB
#B
#BB
$IB
$IB
$IB
$B
$IB
$B
#BB
#BB
#�B
$}B
$�B
$�B
%�B
%�B
& B
&UB
&UB
&UB
&UB
&�B
&�B
'�B
'[B
&�B
&�B
'[B
''B
''B
'�B
'�B
(aB
(�B
(�B
+B
*�B
*�B
+?B
+B
*�B
*�B
*�B
*9B
*9B
*B
)�B
)gB
)�B
)�B
)�B
*�B
+?B
+?B
*�B
*�B
+?B
,B
-�B
.�B
.�B
.�B
.�B
.�B
/XB
/�B
/�B
/�B
/�B
/�B
10B
10B
1�B
2B
2B
26B
1�B
2B
3B
3B
3pB
3<B
3�B
4B
3�B
4�B
5}B
5�B
5�B
6B
6�B
6�B
7TB
7�B
7 B
7TB
7�B
8[B
8�B
9,B
9,B
9,B
8�B
9,B
8�B
9,B
9,B
9�B
:�B
;9B
;B
;mB
;�B
<
B
<?B
=yB
>�B
>�B
?B
?�B
?�B
?�B
@#B
@�B
@�B
A^B
A)B
A�B
A^B
A^B
A�B
B/B
B�B
B�B
B�B
CjB
C�B
DB
D<B
D�B
D�B
D�B
EB
EB
EB
EBB
EBB
EvB
EvB
EvB
E�B
E�B
E�B
FB
F�B
GB
GB
G�B
G�B
G�B
H�B
H�B
H B
H�B
H�B
H�B
H�B
I&B
I&B
I&B
I&B
I�B
I�B
IZB
I�B
I�B
J`B
J�B
J�B
J�B
J�B
KgB
K�B
K�B
K�B
K�B
L8B
LmB
L�B
L�B
M>B
M�B
NEB
NEB
NEB
NEB
NyB
N�B
OB
N�B
OB
OB
OKB
O�B
P�B
Q�B
QWB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R]B
R]B
R�B
S�B
TB
TB
TB
UB
U;B
U�B
UpB
U�B
VB
U�B
U�B
VB
VB
VAB
VB
VAB
V�B
VvB
V�B
WB
W|B
WGB
W�B
W�B
XB
XNB
X�B
X�B
X�B
YB
YTB
YTB
ZZB
Z�B
Z�B
Z�B
[,B
[,B
[,B
[,B
[`B
[�B
[�B
[�B
[�B
\2B
\2B
\fB
\�B
]B
]8B
]8B
]lB
]�B
]�B
^
B
^rB
^�B
^�B
^�B
^�B
_B
^�B
_B
_DB
_�B
_�B
_�B
_yB
_DB
_B
_B
_B
_DB
_yB
_yB
_�B
`B
`�B
`�B
`�B
`�B
aB
aB
aB
`�B
aB
`�B
aB
`�B
`�B
aB
aB
aB
aPB
aB
aB
aPB
aPB
aPB
aB
aPB
aPB
aPB
a�B
bWB
b�B
c�B
c�B
c�B
c�B
c�B
c�B
d.B
e B
e B
e5B
e5B
eiB
eiB
e�B
eiB
e5B
foB
gAB
g�B
hB
hB
hGB
hGB
hGB
h{B
h�B
iB
i�B
i�B
jB
jB
jSB
jSB
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
kYB
k�B
k�B
l+B
l�B
l`B
l�B
l�B
l�B
l�B
l�B
l�B
mfB
m�B
m�B
nlB
nlB
nlB
n�B
n�B
orB
o>B
o�B
o�B
o�B
o�B
pDB
pxB
pxB
pxB
qB
q~B
q�B
rB
q�B
q�B
q�B
q�B
q�B
rPB
rPB
r�B
r�B
r�B
r�B
s�B
s�B
t(B
t�B
t�B
t�B
t�B
t�B
ubB
u.B
ubB
u�B
u�B
v B
v4B
viB
viB
viB
v�B
w:B
woB
woB
woB
woB
woB
x@B
x@B
x@B
x@B
x�B
x�B
x�B
yB
yGB
y�B
y�B
y�B
zMB
z�B
z�B
{B
{�B
{�B
|%B
|�B
|YB
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
~1B
}�B
~�B
~�B
7B
kB
�B
�=B
�rB
�rB
��B
��B
�B
�CB
�CB
�xB
�xB
��B
��B
�B
��B
�B
�=B
�B
��B
�qB
�}B
��B
�B
�B
�B
�}B
�B
�CB
��B
��B
��B
�=B
�IB
�IB
�qB
��B
�B
��B
�IB
�kB
��B
�=B
��B
��B
�=B
�CB
��B
�=B
��B
�kB
��B
��B
�B
�B
�CB
�B
�7B
�B
�kB
�B
�=B
��B
��B
��B
��B
��B
�wB
�kB
�7B
��B
��B
��B
�7B
�kB
��B
�qB
�0B
�qB
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�YB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�_B
�$B
�$B
��B
�B
��B
��B
��B
�B
�YB
��B
�RB
��B
�RB
��B
��B
��B
��B
�$B
��B
��B
��B
��B
��B
��B
�*B
��B
��B
��B
�YB
��B
�*B
��B
��B
��B
��B
�YB
�_B
�_B
�YB
�0B
��B
��B
�0B
��B
�*B
��B
�YB
�eB
�*B
��B
��B
��B
�*B
�0B
��B
��B
��B
�YB
�*B
��B
��B
�*B
��B
�YB
��B
��B
�$B
�0B
��B
�_B
�eB
��B
��B
��B
�YB
�0B
�_B
��B
��B
�YB
��B
��B
��B
��B
�$B
��B
�*B
�$B
��B
�$B
��B
�$B
��B
�_B
��B
�_B
��B
��B
��B
��B
��B
��B
��B
�*B
�YB
��B
��B
�$B
��B
�*B
��B
��B
��B
��B
�YB
��B
��B
��B
�B
�$B
�YB
��B
�$B
��B
��B
��B
�LB
�$B
��B
��B
��B
��B
�{B
��B
��B
��B
��B
�B
��B
��B
�.B
�bB
��B
�!B
��B
��B
�B
�B
��B
�B
��B
�7B
�zB
�^B
��B
�B
��B
�*B
��B
�$B
��B
��B
��B
�RB
��B
�RB
��B
�LB
�zB
�}B
�[B
�<B
��B
�BB
��B
�B
�EB
�B
�9B
�}B
�B
�B
�6B
�B
�B7B!BUB!B10B26B7�BG�BGBK2BP�BOKBPBVABZ�BTBK�BG�BI�BIZBFHBF�BG�BI�BM>BJ,BJ,BK�BK2B?�BH�BI�BJ�BM
BM>BL�BJ�BK�BKgBK2BL8BPBNEBM�BOKBR�BT�BW|Be Bd�BbWBk%Bl�Bl`Bj�Bl`Bl�Bj�Bk%Bl`Bk�Bk%Bl�Bo	BnlBo	BwBz�B}_B~�B~eB|�B~�B|�B�!B��B�RB��B��B�YB��B��B�dB˛B��B��B��BȽB�B��B�^BĤB��B��B��B��B��B��B۔BڎB��B��B�,B�<B�B�B�OB��B��B�B��B�RB��B�bB�Bz�Bx�Bv�Bw�B~1Bv4Bw�Bt(Bt�Bt\Bt\Bt(BrPBs�B��B}+BguBk�BguBguBl�B��B�Bh�Bb�BV�BU;BT�BT5BQ�BO�BOKBJ�BG�BG�BJ`BFBHTBH�BA�BD<BBdB?�B>�B=EBEB>�BGBB�Bb�BR�BQWBP�B^>B]8Bc�B\fBj�BfBa�B_�B_�B_B`Ba�Bl+Bp�B]BT5BJ�B?QB:�BFB>KB9�B9aB5BD�BL�BT�BLmBK�BCjB>�B:�B9�B:�B;9B<�B?�B>�B;�B;B:3B6NB6B9�B=yB9�B<?B6�B5B3�B2B2jB2�B6NBZZBG�BM
BK�BXNB]8B`Bd�Bf;BjSBc]Ba�BZZB\�Ba�B[�BTBPQBQ#BU�BPQBScBTBR�BU;BY�BZZBVBR�BY�BUBTBQ�BXNBT�BR�BZ�B\�BO�BL8BI�BG�BFHBD<BA�B@�B?QBA�B?�B>�B;mBDB<�B/�B1�B)gB*mB&�B,�B!�B�B$B�B�B�B�B dB�B$IB�B-B�BIB dB�B7B
{BLB�B}BLB
FBPB �B
�B
��B
�B
�B
�B
�B
��B
��B
ܛB
��B
˛B
��B
ǷB
��B
�WB
�mB
�
B
�TB
�9B
��B
�0B
��B
��B
�B
��B
�B
��B
��B
��B
��B
�:B
��B
�B
}+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230710070106                            20230710070106AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023071007010620230710070106  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023071007010620230710070106QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023071007010620230710070106QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               