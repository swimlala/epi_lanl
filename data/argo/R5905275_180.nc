CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:44Z creation      
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
_FillValue                 �  [�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ch   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � @   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � hP   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �P   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �P   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �P   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �P   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230426223244  20230426223244  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @��FCe@��FCe11  @��Հ@��Հ@,G�(�߹@,G�(�߹�c&6�F �c&6�F 11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AB  AB  ?�  @   @@  @�G�@�  @�p�@�  A   A  A ��A,��A?\)A_\)A\)A��A�  A�  A��A�\)A�  A�Q�B (�B(�B(�B(�B   B'�B0  B8  B?�
BG�
BP(�BXQ�B`Q�Bg�
Bp  Bx  B�  B�  B�{B�{B�  B�  B�  B�  B�  B�{B�  B��B�  B�  B��B��B��B�  B�  B��B��
B��B�  B��B��B��B�  B�{B�  B�(�B�{B�  B��C  C
=C  C  C

=C
=C  C  C
=C
=C
=C  C  C  C  C   C!��C#��C%��C(  C*  C,  C.
=C0
=C2  C4  C5��C7��C:  C;��C>  C@  CA��CD
=CF  CG��CJ  CK��CM�CO��CR  CT  CU��CW�CY��C[��C^  C`
=Cb
=Cd  Cf  Ch  Cj
=Cl  Cn  Cp  Cr
=Ct{Cv
=Cx
=Cz
=C|  C~
=C�C�  C�  C�  C�  C�  C�  C�C�  C���C�C�  C���C���C���C�  C�
=C�C�  C���C�  C�  C���C���C�  C�C�  C�
=C�  C�  C�
=C�C���C�  C���C���C���C�  C�  C�C�C�C�
=C�
=C�  C���C���C�  C�  C�C�
=C�C�  C�C�  C�  C�C�C�  C�
=C�C�  C���C���C�  C�  C�  C���C���C���C���C�  C�C�  C���C�C�  C���C���C���C���C���C���C���C���C���C�  C�
=C�
=C���C���C���C���C���C���C�C�C�  C�  C�  C�C�C�C���C���C���C���C���C���C�  C�C���C�  C�  C���C�  C�C�  C���C�  C�C�  C�  C�C�
=C�C�C�  D   D }qD ��DxRD�qD��D�D��D�D��D�D��D�D��DD��D�qD� D	�D	�D
�D
}qD�D��D  Dz�D�qD}qD�D}qD��D}qD  D�D�D��D  D}qD�D� D�qD��D�D� D�D� D  D}qD�qD� D  D� D  D� D  D� D�D� D  D��D�D� D��D}qD   D � D!  D!� D!�qD"}qD#�D#��D$  D$� D%�D%� D%�qD&��D'�D'� D'�qD(� D)�D)� D)�qD*� D+  D+}qD,  D,� D-  D-}qD-��D.}qD/  D/��D0  D0� D1�D1��D1�qD2}qD3  D3��D4�D4� D4�qD5z�D6  D6� D7  D7��D7�qD8� D9�D9� D:  D:� D;  D;��D<�D<��D=�D=��D>  D>� D?�D?��D@  D@}qD@��DA}qDB�DB��DC�DC� DD  DD� DE  DE}qDE��DF}qDG�DG� DH  DH� DI  DI� DI�qDJ}qDK  DK� DL�DL}qDL��DM}qDN�DN� DO  DO}qDO�qDP� DQ�DQ��DR  DR}qDS  DS� DS�qDT� DU  DU� DV  DV� DW�DW}qDW�qDX� DY  DY}qDZ  DZ}qDZ�qD[� D\�D\��D]�D]� D]�qD^� D_  D_� D`�D`��Da�Da��Da�qDb}qDc  Dc� Dd  Dd� Dd�qDe� Df  Df� Df�qDg}qDh  Dh��Di�Di� Di�qDj}qDk  Dk}qDk�qDl� Dl�qDm}qDn�Dn� Do  Do}qDo�qDp� Dq�Dq� Dr  Dr��Ds�Ds� Dt  Dt��Du  Du� DvDv��Dw�Dw�Dx�Dx� Dy�Dy��Dy�qDz}qDz�qD{}qD|  D|� D}�D}� D~  D~}qD~�qD}qD��D�=qD�� D�� D��qD�>�D�}qD�� D�HD�@ D�� D�� D�  D�>�D�~�D���D�  D�AHD�� D���D�  D�@ D�}qD�� D�HD�@ D�� D�� D�  D�@ D��HD��HD�HD�B�D�� D��qD��qD�@ D�� D��HD�  D�>�D��HD�� D��qD�@ D�~�D��qD�  D�AHD��HD�� D�HD�B�D��HD���D�  D�AHD��HD�� D�HD�AHD���D��HD�  D�>�D�}qD�� D�HD�@ D�~�D��HD�  D�=qD�� D��HD�HD�@ D�}qD���D�  D�@ D��HD�D�HD�=qD�}qD���D���D�>�D�~�D�� D��D�AHD��HD�� D�  D�>�D�}qD�� D�HD�@ D��HD�D�HD�@ D��HD�D�  D�AHD���D��HD�HD�AHD�� D�� D�HD�AHD��HD�� D���D�=qD�� D��HD�  D�@ D���D�D�HD�AHD��HD�� D�  D�@ D�}qD���D��qD�=qD�� D�� D�HD�B�D�~�D�� D�  D�@ D��HD���D�  D�AHD�� D��qD��qD�@ D��HD�� D�  D�AHD�� D�� D�  D�>�D�� D�� D�HD�B�D�� D���D���D�@ D�~�D���D�  D�AHD�� D���D�  D�B�D��HD�� D�HD�B�D���D�� D��qD�@ D���D�� D�  D�@ D�}qD��qD��qD�>�D�~�D��qD�  D�@ D�|)D��)D��qD�>�D�� D�� D�  D�@ D�~�D��qD��qD�=qD�~�D�� D�  D�AHD���D�D��D�C�D��HD��HD�  D�>�D�� D��HD��D�@ D�~�D��HD�  D�>�D��HD�� D���D�>�D�� D�D�HD�@ D��HD��HD�  D�AHD���D�D�  D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��HD�� D�  D�AHD��HD��HD�HD�@ D D¾�D���D�>�DÀ Dþ�D��qD�>�DĀ D�� D�HD�B�Dŀ Dž�D�  D�>�D�~�D��HD�HD�@ Dǀ D�� D���D�>�DȁHD��HD�HD�>�D�~�D�� D�  D�@ D�~�Dʾ�D�  D�AHDˁHD˾�D�  D�@ D�}qD�� D��D�B�D̀ D;�D�  D�@ D΀ Dξ�D�  D�AHDρHD�� D�HD�B�DЀ D�� D�  D�>�D�~�DѾ�D���D�@ DҀ D��HD��D�B�D�~�DӽqD���D�>�D�~�D�D�  D�>�DՁHD�� D�  D�@ Dր D־�D�  D�AHD׀ D��HD�HD�@ D�~�DؽqD�  D�>�D�~�Dپ�D���D�>�Dڀ D�� D�HD�AHDۂ�D�D�HD�@ D܀ Dܾ�D���D�>�D݀ D�D��D�B�Dހ D޽qD��qD�>�D߀ D�� D�  D�AHD�� DྸD�  D�>�D�}qDᾸD�HD�B�D�HD⾸D��qD�@ D�~�D㾸D�HD�AHD�HD�D�HD�AHD傏D��HD���D�@ D� D��HD�HD�@ D�HD��HD�  D�=qD�~�D��HD�HD�AHD邏D�� D���D�@ D� D�� D�HD�AHD�HD��HD��D�@ D� D�� D�  D�@ D�}qD��qD���D�>�D�HD��HD�HD�@ D�~�D�� D���D�@ D�~�D�� D���D�>�D� D��HD�  D�@ D�HD�� D�  D�AHD� D�D�  D�AHD� D���D�HD�B�D��HD�� D�  D�@ D�~�D���D�  D�AHD�� D���D�HD�@ D�~�D��HD�  D�@ D��HD�D�  D�>�D�� D�� D�  D�0�D�q�?\)?B�\?u?��R?��?�@�@�@#�
@5@L��@\(�@n{@}p�@�ff@�\)@��H@��
@��@�33@�p�@Ǯ@У�@ٙ�@�G�@���@�33@�(�A�\AffA
�HA\)AA=qA{A"�\A'�A,��A1�A7
=A;�A@��AEAJ�HAP��AU�AY��A_\)Ae�Aj=qAn�RAs33AxQ�A~{A��A�z�A�
=A���A��
A��RA�G�A��A�p�A�  A��HA�A�Q�A�=qA�z�A�
=A��A�z�A��RA�G�A�33A�p�A�  A��HA��A�
=A���AÅA�A�Q�Aʏ\A�z�AθRA���AӅA�{A���A�33A�A�Q�A�33A�ffA���A�A�{A��A��
A��RA�G�A��A�{B z�B�B33Bz�BB
=Bz�B	�B\)Bz�BB
=Bz�B�B
=BQ�Bp�B�RB(�Bp�B�RB�
B�B=qB�B!�B"=qB#\)B$z�B%B'
=B(Q�B)��B*�RB,  B-G�B.=qB/�B0��B2=qB3\)B4��B5B7
=B8Q�B9��B;
=B<Q�B=��B>�RB@  BAp�BB�RBD(�BEp�BF�HBHQ�BI��BJ�HBL(�BM��BO
=BPz�BQ�BS\)BT��BU�BW33BX(�BYG�BZ{B[33B\  B\��B]��B^{B^�RB_33B_�B`Q�B`��BaG�Ba��Ba�Bb=qBb�RBc33Bc�Bd(�Bd��Be�BeBf{BfffBf�RBg33Bg�
BhQ�Bh��BiG�BiBj{Bj�\Bk33Bk�
BlQ�Bl��BmG�BmBn=qBn�RBo\)Bo�
Bpz�Bq�BqBr=qBr�RBs33Bs�BtQ�Bt��Bu��Bv{Bv�HBw\)Bx(�Bx��Byp�Bz{Bz�RB{33B{�
B|z�B}�B}�B~�RB33B�
B�=qB��\B��HB�33B��B��B�=qB���B�
=B�\)B��B��B�=qB�z�B��HB�33B���B��B�=qB��\B��HB�33B�p�B�B�  B�Q�B���B���B�G�B���B��B�=qB��\B��HB�
=B�\)B���B��B�(�B��\B��HB�33B��B��
B�(�B�z�B��RB���B�G�B���B��B�Q�B���B���B�G�B���B��B�(�B��\B��HB�33B���B��B�Q�B���B���B�G�B���B��B�=qB��\B��HB�33B���B�  B�Q�B��RB��B�p�B��
B�(�B�z�B��HB�33B�p�B�B�{B�ffB���B��B�p�B�B�(�B�z�B��HB�G�B���B�  B�ffB���B��B�p�B��
B�(�B��\B���B�33B���B�  B�ffB���B�33B���B�  B�ffB���B�33B��B��B�Q�B���B�
=B�p�B��
B�=qB���B�
=B�p�B��
B�=qB��RB��B���B�  B�z�B��HB�p�B��
B�ffB���B�G�B�B�=qB���B�G�B��B�(�B���B��B���B�{B��\B�
=B��B�  B�z�B�
=B���B�{B���B�33B���B�(�B��RB�G�B��
B�Q�B���B�\)B��B�z�B���B��B�{B£�B��BîB�=qB���B�\)B��B�ffB�
=BǅB�{Bȣ�B�33B�B�Q�B��HB�p�B�  B̏\B��BͮB�=qB��HB�p�B�  BЏ\B��B�B�Q�B��HBӅB�{Bԣ�B�G�B��
B�ffB�
=Bי�B�Q�B��HBمB�(�BڸRB�\)B��B܏\B��B�B�Q�B�
=B߮B�=qB��HB�p�B�{B�RB�\)B��B�\B��B�B�Q�B���B�B�{B���B�\)B��B�\B��B�B�Q�B��HB�p�B�  B��B�33B��
B�ffB�
=B�B�=qB���B�p�B�  B���B�33B�B�Q�B��HB��B�{B���B��B��B�=qB���B�G�B��
B�ffB���B��B�{B���B�33B�C (�C p�C �RC
=CQ�C��C�C(�Cp�C�RC  CG�C�C�
C{CffC��C�C33Cz�CC
=CQ�C��C�HC33Cp�CC  CG�C�\C�
C	�C	\)C	��C	�HC
(�C
p�C
�RC  CG�C�\C��C(�CffC�C  CG�C�C��C{C\)C��C�HC(�CffC�C��C=qC�\C�
C�C\)C�C�C33Cp�C�C�C33Cp�C�RC  CQ�C��C�HC(�CffC�C�C33Cp�C�C�C33Cp�C�RC  CG�C�\C��C
=CQ�C�\C��C
=CG�C�C��C{C\)C��C��C=qC�C��C{CQ�C��C�HC33C�C�
C�Cp�C�RC   C =qC �C �
C!�C!p�C!C"
=C"\)C"��C"�HC#(�C#p�C#�RC$  C$Q�C$��C$�C%33C%�C%C&{C&\)C&��C&�C'33C'�C'��C(�C(p�C(��C){C)ffC)�C*  C*G�C*��C*�HC+33C+�C+�HC,33C,�C,��C-�C-p�C-�RC.
=C.Q�C.��C.��C/Q�C/�C0  C0Q�C0��C0�C1G�C1�\C1�HC233C2�\C2�C333C3�C3�
C4�C4p�C4C5
=C5ffC5�RC6{C6ffC6�C7  C7Q�C7��C7�C8G�C8��C8�C9=qC9�\C9�HC:33C:z�C:C;{C;ffC;C<{C<ffC<�RC=
=C=Q�C=�\C=�HC>33C>�C>�HC?33C?z�C?��C@{C@\)C@�CA  CAQ�CA�CB  CBQ�CB��CB�CC33CC�CC�HCD33CD�CD�
CE�CEffCE�RCF{CFffCF�RCG  CGG�CG��CG��CHG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                 ?�  @   @@  @�G�@�  @�p�@�  A   A  A ��A,��A?\)A_\)A\)A��A�  A�  A��A�\)A�  A�Q�B (�B(�B(�B(�B   B'�B0  B8  B?�
BG�
BP(�BXQ�B`Q�Bg�
Bp  Bx  B�  B�  B�{B�{B�  B�  B�  B�  B�  B�{B�  B��B�  B�  B��B��B��B�  B�  B��B��
B��B�  B��B��B��B�  B�{B�  B�(�B�{B�  B��C  C
=C  C  C

=C
=C  C  C
=C
=C
=C  C  C  C  C   C!��C#��C%��C(  C*  C,  C.
=C0
=C2  C4  C5��C7��C:  C;��C>  C@  CA��CD
=CF  CG��CJ  CK��CM�CO��CR  CT  CU��CW�CY��C[��C^  C`
=Cb
=Cd  Cf  Ch  Cj
=Cl  Cn  Cp  Cr
=Ct{Cv
=Cx
=Cz
=C|  C~
=C�C�  C�  C�  C�  C�  C�  C�C�  C���C�C�  C���C���C���C�  C�
=C�C�  C���C�  C�  C���C���C�  C�C�  C�
=C�  C�  C�
=C�C���C�  C���C���C���C�  C�  C�C�C�C�
=C�
=C�  C���C���C�  C�  C�C�
=C�C�  C�C�  C�  C�C�C�  C�
=C�C�  C���C���C�  C�  C�  C���C���C���C���C�  C�C�  C���C�C�  C���C���C���C���C���C���C���C���C���C�  C�
=C�
=C���C���C���C���C���C���C�C�C�  C�  C�  C�C�C�C���C���C���C���C���C���C�  C�C���C�  C�  C���C�  C�C�  C���C�  C�C�  C�  C�C�
=C�C�C�  D   D }qD ��DxRD�qD��D�D��D�D��D�D��D�D��DD��D�qD� D	�D	�D
�D
}qD�D��D  Dz�D�qD}qD�D}qD��D}qD  D�D�D��D  D}qD�D� D�qD��D�D� D�D� D  D}qD�qD� D  D� D  D� D  D� D�D� D  D��D�D� D��D}qD   D � D!  D!� D!�qD"}qD#�D#��D$  D$� D%�D%� D%�qD&��D'�D'� D'�qD(� D)�D)� D)�qD*� D+  D+}qD,  D,� D-  D-}qD-��D.}qD/  D/��D0  D0� D1�D1��D1�qD2}qD3  D3��D4�D4� D4�qD5z�D6  D6� D7  D7��D7�qD8� D9�D9� D:  D:� D;  D;��D<�D<��D=�D=��D>  D>� D?�D?��D@  D@}qD@��DA}qDB�DB��DC�DC� DD  DD� DE  DE}qDE��DF}qDG�DG� DH  DH� DI  DI� DI�qDJ}qDK  DK� DL�DL}qDL��DM}qDN�DN� DO  DO}qDO�qDP� DQ�DQ��DR  DR}qDS  DS� DS�qDT� DU  DU� DV  DV� DW�DW}qDW�qDX� DY  DY}qDZ  DZ}qDZ�qD[� D\�D\��D]�D]� D]�qD^� D_  D_� D`�D`��Da�Da��Da�qDb}qDc  Dc� Dd  Dd� Dd�qDe� Df  Df� Df�qDg}qDh  Dh��Di�Di� Di�qDj}qDk  Dk}qDk�qDl� Dl�qDm}qDn�Dn� Do  Do}qDo�qDp� Dq�Dq� Dr  Dr��Ds�Ds� Dt  Dt��Du  Du� DvDv��Dw�Dw�Dx�Dx� Dy�Dy��Dy�qDz}qDz�qD{}qD|  D|� D}�D}� D~  D~}qD~�qD}qD��D�=qD�� D�� D��qD�>�D�}qD�� D�HD�@ D�� D�� D�  D�>�D�~�D���D�  D�AHD�� D���D�  D�@ D�}qD�� D�HD�@ D�� D�� D�  D�@ D��HD��HD�HD�B�D�� D��qD��qD�@ D�� D��HD�  D�>�D��HD�� D��qD�@ D�~�D��qD�  D�AHD��HD�� D�HD�B�D��HD���D�  D�AHD��HD�� D�HD�AHD���D��HD�  D�>�D�}qD�� D�HD�@ D�~�D��HD�  D�=qD�� D��HD�HD�@ D�}qD���D�  D�@ D��HD�D�HD�=qD�}qD���D���D�>�D�~�D�� D��D�AHD��HD�� D�  D�>�D�}qD�� D�HD�@ D��HD�D�HD�@ D��HD�D�  D�AHD���D��HD�HD�AHD�� D�� D�HD�AHD��HD�� D���D�=qD�� D��HD�  D�@ D���D�D�HD�AHD��HD�� D�  D�@ D�}qD���D��qD�=qD�� D�� D�HD�B�D�~�D�� D�  D�@ D��HD���D�  D�AHD�� D��qD��qD�@ D��HD�� D�  D�AHD�� D�� D�  D�>�D�� D�� D�HD�B�D�� D���D���D�@ D�~�D���D�  D�AHD�� D���D�  D�B�D��HD�� D�HD�B�D���D�� D��qD�@ D���D�� D�  D�@ D�}qD��qD��qD�>�D�~�D��qD�  D�@ D�|)D��)D��qD�>�D�� D�� D�  D�@ D�~�D��qD��qD�=qD�~�D�� D�  D�AHD���D�D��D�C�D��HD��HD�  D�>�D�� D��HD��D�@ D�~�D��HD�  D�>�D��HD�� D���D�>�D�� D�D�HD�@ D��HD��HD�  D�AHD���D�D�  D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��HD�� D�  D�AHD��HD��HD�HD�@ D D¾�D���D�>�DÀ Dþ�D��qD�>�DĀ D�� D�HD�B�Dŀ Dž�D�  D�>�D�~�D��HD�HD�@ Dǀ D�� D���D�>�DȁHD��HD�HD�>�D�~�D�� D�  D�@ D�~�Dʾ�D�  D�AHDˁHD˾�D�  D�@ D�}qD�� D��D�B�D̀ D;�D�  D�@ D΀ Dξ�D�  D�AHDρHD�� D�HD�B�DЀ D�� D�  D�>�D�~�DѾ�D���D�@ DҀ D��HD��D�B�D�~�DӽqD���D�>�D�~�D�D�  D�>�DՁHD�� D�  D�@ Dր D־�D�  D�AHD׀ D��HD�HD�@ D�~�DؽqD�  D�>�D�~�Dپ�D���D�>�Dڀ D�� D�HD�AHDۂ�D�D�HD�@ D܀ Dܾ�D���D�>�D݀ D�D��D�B�Dހ D޽qD��qD�>�D߀ D�� D�  D�AHD�� DྸD�  D�>�D�}qDᾸD�HD�B�D�HD⾸D��qD�@ D�~�D㾸D�HD�AHD�HD�D�HD�AHD傏D��HD���D�@ D� D��HD�HD�@ D�HD��HD�  D�=qD�~�D��HD�HD�AHD邏D�� D���D�@ D� D�� D�HD�AHD�HD��HD��D�@ D� D�� D�  D�@ D�}qD��qD���D�>�D�HD��HD�HD�@ D�~�D�� D���D�@ D�~�D�� D���D�>�D� D��HD�  D�@ D�HD�� D�  D�AHD� D�D�  D�AHD� D���D�HD�B�D��HD�� D�  D�@ D�~�D���D�  D�AHD�� D���D�HD�@ D�~�D��HD�  D�@ D��HD�D�  D�>�D�� D�� D�  D�0�D�q�?\)?B�\?u?��R?��?�@�@�@#�
@5@L��@\(�@n{@}p�@�ff@�\)@��H@��
@��@�33@�p�@Ǯ@У�@ٙ�@�G�@���@�33@�(�A�\AffA
�HA\)AA=qA{A"�\A'�A,��A1�A7
=A;�A@��AEAJ�HAP��AU�AY��A_\)Ae�Aj=qAn�RAs33AxQ�A~{A��A�z�A�
=A���A��
A��RA�G�A��A�p�A�  A��HA�A�Q�A�=qA�z�A�
=A��A�z�A��RA�G�A�33A�p�A�  A��HA��A�
=A���AÅA�A�Q�Aʏ\A�z�AθRA���AӅA�{A���A�33A�A�Q�A�33A�ffA���A�A�{A��A��
A��RA�G�A��A�{B z�B�B33Bz�BB
=Bz�B	�B\)Bz�BB
=Bz�B�B
=BQ�Bp�B�RB(�Bp�B�RB�
B�B=qB�B!�B"=qB#\)B$z�B%B'
=B(Q�B)��B*�RB,  B-G�B.=qB/�B0��B2=qB3\)B4��B5B7
=B8Q�B9��B;
=B<Q�B=��B>�RB@  BAp�BB�RBD(�BEp�BF�HBHQ�BI��BJ�HBL(�BM��BO
=BPz�BQ�BS\)BT��BU�BW33BX(�BYG�BZ{B[33B\  B\��B]��B^{B^�RB_33B_�B`Q�B`��BaG�Ba��Ba�Bb=qBb�RBc33Bc�Bd(�Bd��Be�BeBf{BfffBf�RBg33Bg�
BhQ�Bh��BiG�BiBj{Bj�\Bk33Bk�
BlQ�Bl��BmG�BmBn=qBn�RBo\)Bo�
Bpz�Bq�BqBr=qBr�RBs33Bs�BtQ�Bt��Bu��Bv{Bv�HBw\)Bx(�Bx��Byp�Bz{Bz�RB{33B{�
B|z�B}�B}�B~�RB33B�
B�=qB��\B��HB�33B��B��B�=qB���B�
=B�\)B��B��B�=qB�z�B��HB�33B���B��B�=qB��\B��HB�33B�p�B�B�  B�Q�B���B���B�G�B���B��B�=qB��\B��HB�
=B�\)B���B��B�(�B��\B��HB�33B��B��
B�(�B�z�B��RB���B�G�B���B��B�Q�B���B���B�G�B���B��B�(�B��\B��HB�33B���B��B�Q�B���B���B�G�B���B��B�=qB��\B��HB�33B���B�  B�Q�B��RB��B�p�B��
B�(�B�z�B��HB�33B�p�B�B�{B�ffB���B��B�p�B�B�(�B�z�B��HB�G�B���B�  B�ffB���B��B�p�B��
B�(�B��\B���B�33B���B�  B�ffB���B�33B���B�  B�ffB���B�33B��B��B�Q�B���B�
=B�p�B��
B�=qB���B�
=B�p�B��
B�=qB��RB��B���B�  B�z�B��HB�p�B��
B�ffB���B�G�B�B�=qB���B�G�B��B�(�B���B��B���B�{B��\B�
=B��B�  B�z�B�
=B���B�{B���B�33B���B�(�B��RB�G�B��
B�Q�B���B�\)B��B�z�B���B��B�{B£�B��BîB�=qB���B�\)B��B�ffB�
=BǅB�{Bȣ�B�33B�B�Q�B��HB�p�B�  B̏\B��BͮB�=qB��HB�p�B�  BЏ\B��B�B�Q�B��HBӅB�{Bԣ�B�G�B��
B�ffB�
=Bי�B�Q�B��HBمB�(�BڸRB�\)B��B܏\B��B�B�Q�B�
=B߮B�=qB��HB�p�B�{B�RB�\)B��B�\B��B�B�Q�B���B�B�{B���B�\)B��B�\B��B�B�Q�B��HB�p�B�  B��B�33B��
B�ffB�
=B�B�=qB���B�p�B�  B���B�33B�B�Q�B��HB��B�{B���B��B��B�=qB���B�G�B��
B�ffB���B��B�{B���B�33B�C (�C p�C �RC
=CQ�C��C�C(�Cp�C�RC  CG�C�C�
C{CffC��C�C33Cz�CC
=CQ�C��C�HC33Cp�CC  CG�C�\C�
C	�C	\)C	��C	�HC
(�C
p�C
�RC  CG�C�\C��C(�CffC�C  CG�C�C��C{C\)C��C�HC(�CffC�C��C=qC�\C�
C�C\)C�C�C33Cp�C�C�C33Cp�C�RC  CQ�C��C�HC(�CffC�C�C33Cp�C�C�C33Cp�C�RC  CG�C�\C��C
=CQ�C�\C��C
=CG�C�C��C{C\)C��C��C=qC�C��C{CQ�C��C�HC33C�C�
C�Cp�C�RC   C =qC �C �
C!�C!p�C!C"
=C"\)C"��C"�HC#(�C#p�C#�RC$  C$Q�C$��C$�C%33C%�C%C&{C&\)C&��C&�C'33C'�C'��C(�C(p�C(��C){C)ffC)�C*  C*G�C*��C*�HC+33C+�C+�HC,33C,�C,��C-�C-p�C-�RC.
=C.Q�C.��C.��C/Q�C/�C0  C0Q�C0��C0�C1G�C1�\C1�HC233C2�\C2�C333C3�C3�
C4�C4p�C4C5
=C5ffC5�RC6{C6ffC6�C7  C7Q�C7��C7�C8G�C8��C8�C9=qC9�\C9�HC:33C:z�C:C;{C;ffC;C<{C<ffC<�RC=
=C=Q�C=�\C=�HC>33C>�C>�HC?33C?z�C?��C@{C@\)C@�CA  CAQ�CA�CB  CBQ�CB��CB�CC33CC�CC�HCD33CD�CD�
CE�CEffCE�RCF{CFffCF�RCG  CGG�CG��CG��CHG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�`BA�`BA�`BA�`BA�`BA�`BA�`BA�bNA�bNA�bNA�bNA�bNA�dZA�ffA�dZA�ffA�ffA�hsA�hsA��A��A���A��#AӶFAӶFAӧ�A�A��A��Aǥ�A���A���A��yA��A���A�?}A���A�  A��A��#A��\A�dZA�ƨA�~�A���A���A�l�A���A�hsA��A�I�A�?}A�7LA�{A�jA�~�A�dZA���A���A�9XA�A�A��A��PA��yA��A�XA��A�
A}ƨAx�+AuC�Ao��AmXAkVAi?}Agx�Ac��AaG�A]
=AYƨAV$�AR�APQ�AM��AK�AI�AI�AIXAHv�AFn�AEdZAF  ADA�AAC�A?`BA=K�A;VA;K�A;�A<A;dZA:r�A9hsA933A9G�A9�-A9+A7�hA7\)A6��A6-A5�^A5K�A4��A3A37LA2v�A2$�A2bA2VA1�A1��A0��A/�TA.jA-�FA-p�A-7LA,JA+��A+33A*��A*��A*�A*�9A*I�A(�A(��A'�7A&ZA%�FA%?}A$�/A$�RA$jA$M�A$ �A#�mA#�A#|�A#dZA#G�A#�A"�A"9XA!�mA!��A!dZA!G�A!%A �A!+A!�wA!�A ��A (�A �9A ~�A  �A�A�A�-AĜA-AffA-A�-AVA^5A(�A|�AoA�!A^5A�A��AVA�uAVA1A�7A7LA+A�A�/A��AM�A{A��A"�A��A�HA�jAz�A�A5?AE�A1'A-A��AAC�A��AjA1'A�A��AhsA"�A;dA&�AĜAQ�A1Al�A%AbNA��A
��A
�A	��A	p�A	XA	"�A	�A	VA�9AVAv�AA�AbA�
A��A`BA�A�`AĜA��AVA�A�wA�hAK�A��A��A�AI�A�AA"�A��A-A�mA�A�A ��A ȴA r�A M�A �@�ȴ@��@�Q�@��@�t�@���@�
=@�J@��@�z�@�\)@�v�@�M�@�`B@�@��@�r�@�"�@�C�@�
=@�v�@�E�@�-@���@�@��@�Z@��m@�l�@�~�@���@홚@�9@�1'@�t�@�X@�h@��@畁@睲@�33@���@�M�@�{@�X@���@�@�D@�Q�@�b@�K�@�+@�^5@���@��`@ߕ�@��@�o@�@޸R@�-@ݺ^@�Ĝ@�  @�C�@���@�n�@��T@�O�@�z�@��@�+@�E�@Չ7@ԃ@�b@�|�@�C�@��@�+@���@�$�@��@���@�Z@�+@�ȴ@�M�@͙�@�&�@�/@�%@̣�@���@�\)@ʸR@�V@ɉ7@�&�@ȓu@�I�@���@ǅ@�o@Ə\@�ff@�5?@Ų-@�Ĝ@�(�@î@�"�@�{@�?}@�Z@��m@��
@��w@�S�@���@�=q@���@�/@���@�Z@��
@�dZ@���@�=q@���@��@��`@��9@�A�@��
@�C�@��@��!@�M�@��@�O�@�A�@���@��@�^5@��^@�&�@��@��@���@�z�@��@�t�@��@���@�@�V@���@���@��u@�9X@��
@�t�@�"�@��H@�-@��T@��#@��7@���@���@�j@�b@�dZ@�o@��@��R@�n�@�J@���@��@�X@���@��u@�9X@���@���@�t�@��@��+@�{@�hs@�/@���@��@��@�t�@�S�@�o@�^5@��@���@��@��D@���@��@��@�C�@���@��!@�~�@�-@��@���@��@�`B@�7L@��/@��@�bN@� �@��w@�
=@��!@�v�@�^5@�J@���@�hs@�?}@�%@��@�z�@�j@�1@��P@��@���@���@�v�@�M�@�-@���@�`B@�/@�%@��/@��9@��u@��D@�z�@�I�@��@��P@�C�@�
=@��y@��!@�ff@��T@��h@��@�p�@�7L@��@��/@��D@�bN@�Q�@�1'@�1@�ƨ@��@��@�dZ@�C�@���@��!@���@�~�@�-@�@���@��-@��h@�x�@�X@�G�@�Ĝ@�r�@�Z@�(�@��m@��P@�K�@�"�@���@��@��R@���@��\@�v�@�ff@�E�@�$�@�@���@��-@���@�p�@�X@�O�@�&�@��@�V@���@�bN@�I�@�(�@��@�ƨ@���@�|�@�l�@�;d@�ȴ@�~�@�=q@�$�@���@�?}@��`@���@��@���@��@�(�@�w@~��@~E�@~{@}�T@}��@}O�@}�@|�j@|�@{S�@{C�@z�@z^5@y��@y�^@yG�@xr�@w��@wl�@w;d@w
=@vȴ@v��@vE�@u��@t��@t�j@t(�@s�F@s��@sdZ@so@r^5@rJ@p��@pr�@pA�@o�@o|�@o�@n��@nV@n@m��@m��@m�@m`B@l�/@l�D@l�@kdZ@k@j��@j�@iG�@h��@hA�@h1'@h �@g�;@g�P@g;d@f��@f��@fE�@f{@e�T@e�h@e�@e?}@d�D@d9X@d�@d1@cƨ@c33@b�@b��@b=q@a�#@a��@ahs@a%@`r�@`b@_��@_K�@_;d@^��@^�+@^5?@]�T@]p�@\�@\�@\9X@[�
@[��@[33@Z�H@Zn�@Y��@Yx�@X��@X�@XA�@X  @W��@W�@V�@V�+@V5?@U@U/@T��@T�j@T�@T(�@S��@Sƨ@S33@S@R�H@Rn�@Q�@Q�^@Qx�@P��@Pb@P  @O�;@O�@OK�@N��@N@M�-@M�@Mp�@MO�@M�@LI�@Kƨ@KdZ@J�@J~�@J=q@JJ@I��@Ix�@I%@H �@G�P@GK�@G+@Fv�@E@D��@D�@DZ@C��@C��@C33@B�H@B~�@A��@A�^@A��@AX@AG�@A&�@@��@@�9@@Q�@?��@?|�@?K�@>�@>�+@=�-@=O�@=/@<�/@<j@;�m@;��@;"�@:��@:~�@:-@9�7@9%@8��@8Q�@8  @7�@7�P@6ȴ@6��@6ff@6$�@5�h@5/@4��@4��@4z�@4Z@49X@3�
@3t�@2�@2=q@2=q@2=q@2-@2-@2�@1��@1��@1G�@1%@0��@0�@/��@/+@.�y@.�@.�R@.V@-�@-��@-�@,�/@,��@,�j@,��@,Z@,9X@,(�@+��@+�
@+t�@+C�@+"�@*�H@*��@*�\@*n�@*=q@)�#@)��@)��@)�7@)7L@(�`@(A�@'�;@'��@'�@&��@&V@&V@&V@&V@&ff@&ff@&V@&E�@%�T@%�-@%`B@$�@$Z@$(�@$�@#��@#�
@#�F@#t�@"�@"�\@"�\@"�\@"~�@"^5@"=q@"-@!��@!��@!hs@!7L@ ��@ �9@ �u@ r�@ b@�@��@l�@;d@��@�+@v�@V@5?@5?@5?@�@@��@�@O�@/@��@��@z�@Z@1@�
@t�@o@�H@��@��@��@�\@~�@^5@�@��@�7@G�@�@Ĝ@�u@�@r�@bN@A�@A�@1'@1'@ �@  @��@\)@K�@�y@ff@E�@E�@{@�@/@�j@I�@�@�@�@�@�m@��@S�@"�@��@n�@^5@=q@J@�^@�7@hs@hs@G�@�`@�9@Q�@b@�;@�w@�P@K�@�@��@��@��@��@��@�y@ȴ@�+@E�@$�@@�@�T@@�-@��@�@�/@�/@��@�j@�jA�^5A�\)A�^5A�bNA�bNA�`BA�^5A�\)A�^5A�bNA�bNA�`BA�^5A�^5A�`BA�bNA�bNA�`BA�^5A�^5A�bNA�bNA�`BA�^5A�^5A�`BA�bNA�bNA�`BA�^5A�bNA�dZA�bNA�`BA�^5A�^5A�bNA�dZA�dZA�`BA�`BA�bNA�dZA�ffA�dZA�bNA�bNA�dZA�ffA�ffA�bNA�bNA�dZA�ffA�hsA�ffA�dZA�bNA�ffA�hsA�ffA�dZA�bNA�dZA�hsA�hsA�ffA�dZA�dZA�ffA�hsA�hsA�ffA�dZA�dZA�ffA�hsA�hsA�hsA�dZA�hsA�jA�l�A�jA�ffA�ffA�hsA�jA�jA�hsA�ffA�jA�M�A���AԑhA�C�A�(�A��A�{A�{A�oA�JA�%A���A���A���A�  A���A��A��yA��#A��
A���A���A�AӸRAӴ9AӴ9AӴ9AӲ-AӴ9AӼjA�A���AӸRAӝ�Aӟ�Aӧ�AӸRAӸRAӮAӛ�AӉ7A�t�A�K�A��AҬA�I�A���A���AхA��TA�A�A�bNAΙ�A�
=A�A͕�A��/A��`A�&�Aɡ�Aȧ�AǬAǼjA���A�t�A�
=AƧ�A�dZA�bNA�S�A��A���A�VA��`A�r�A�ZA�?}A�-A�VA��A��`A��;A��/A��/A��HA��/A���A��jA��-A���A���A�n�A�A�A��A��mA��jA��PA�hsA�Q�A�A�A�5?A�(�A��A�A��A��
A���A�ĜA���A��wA��RA��9A��A���A���A���A��uA�S�A��7A�%A���A���A�x�A�M�A�C�A�?}A�"�A�A��A���A���A���A��\A�~�A�`BA�9XA�(�A��A�A��A�p�A�  A�A���A��DA�^5A�7LA�A��DA�=qA��A�A���A��+A��7A�x�A�`BA�A�A�bA��A�5?A��A�
=A��A���A��uA�r�A�hsA�ZA�?}A�+A�(�A��A�JA���A��A��;A�ƨA��wA��^A���A��PA��A�G�A���A�%A�1'A��A��A��+A���A��yA��-A��DA�dZA�=qA���A��A�`BA�/A���A��RA�p�A�"�A���A��A�I�A� �A��A��HA��
A���A���A�ƨA�ȴA�ȴA�ȴA�ȴA�A��A�z�A�I�A�$�A���A���A��hA�jA�VA�A�A��A��TA��A�t�A�C�A��A���A���A���A�l�A�K�A�9XA�33A�/A�/A�+A��A�%A��A��/A�A���A�v�A�7LA��A��A���A���A���A��wA��A���A��A�M�A��A��RA��A�9XA���A���A���A��#A���A��RA���A�|�A�O�A�+A���A�ĜA�v�A�?}A�(�A�bA���A���A�p�A��HA�I�A��A��FA�jA�JA���A�z�A�K�A�K�A�ZA�v�A�x�A�O�A� �A�%A��HA���A���A�t�A�Q�A�(�A�ȴA�dZA�E�A�&�A�5?A�9XA�=qA�;dA�1'A�1'A�5?A�M�A�ZA�E�A��A���A��uA�\)A��A��HA���A�K�A���A���A�\)A��A���A���A�ffA�1'A�1A��A���A��!A��7A�bNA�9XA�%A��^A�E�A���A��#A��!A�ffA��A��hA�ffA�E�A��A�ȴA�ZA��^A�\)A�JA��A�`BA�{A���A�t�A��TA�S�A��7A�VA��
A�r�A���A�p�A�VA�/A���A��A�33A���A�Q�A��jA�~�A�5?A���A���A�~�A�K�A�bA��mA��RA�n�A�$�A���A��A���A�33A��A��RA�r�A�/A��A��uA�&�A��^A�1A���A�\)A�oA��TA��jA���A�r�A�ZA�33A�bA�A��At�A&�A~��A~v�A~JA}��A}�FA}�A|ĜA{ƨAz��Ax�AxbNAw��Av��Av��Av�uAvr�Av5?Au�Aul�At�\As�-Ar�/Aq�TAq33Ap{Ao33An�AnZAn1Am�FAm�AmdZAmS�Am7LAmoAl�RAl=qAk�;Akl�Ak;dAj�jAj1'Ai�-Ai��Ai�7AidZAi7LAi�Ai�AioAiAh�Ah�9AhA�AgAf�9Ae�^Ae+Ad��Ad^5Ad  Ac��Ac7LAb�DAb  Aa��Aa��Aa�wAa�Aa�A`�yA_�A^v�A]�
A]t�A]"�A\�A\��A\9XA[��A[+AZ�+AZ$�AY��AYt�AX��AX^5AW��AWƨAW�AW
=AV5?AT�AT(�AS�
AS��AS\)AS�AR��ARZAQ�AQ��AQ&�AP��AP��AP~�APZAP�AO�;AO��AOdZAO"�AN��AM��AL��AL��AL�ALE�AL-AL-AL �AJz�AI�-AI�7AI�7AI�AI�AI�AI�AI�AI�AI|�AI|�AI�AI�AI�AI�AI�AIt�AIp�AIhsAIXAIG�AI7LAI�AI
=AH��AHȴAH��AHVAG�TAGG�AF�RAF��AFv�AF=qAF1'AF1'AF{AEx�AE33AE�AEAEl�AE�mAF5?AE�
AEAE�AF{AF �AE�AEoAD�!AD�+AD9XAC;dABAA�AA�
AAp�A@�A@��A@�\A@�A?ƨA?�PA?"�A>��A>n�A>(�A=�TA=��A=+A<�A<E�A;�A;7LA:��A:�A:�yA:�`A:�A;%A;"�A;C�A;l�A;��A;�FA;��A;�TA;�A<  A<{A<{A<bA<1A<1A<1A<A;�A;�
A;�FA;�PA;XA;"�A;33A;/A;�A:�A:��A:^5A:{A9�;A9�-A9�hA9|�A9dZA9S�A97LA9/A9+A9+A9/A933A9;dA9?}A9;dA9;dA9;dA9?}A9G�A9XA9p�A9��A9��A9ƨA9��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                 A�`BA�`BA�`BA�`BA�`BA�`BA�`BA�bNA�bNA�bNA�bNA�bNA�dZA�ffA�dZA�ffA�ffA�hsA�hsA��A��A���A��#AӶFAӶFAӧ�A�A��A��Aǥ�A���A���A��yA��A���A�?}A���A�  A��A��#A��\A�dZA�ƨA�~�A���A���A�l�A���A�hsA��A�I�A�?}A�7LA�{A�jA�~�A�dZA���A���A�9XA�A�A��A��PA��yA��A�XA��A�
A}ƨAx�+AuC�Ao��AmXAkVAi?}Agx�Ac��AaG�A]
=AYƨAV$�AR�APQ�AM��AK�AI�AI�AIXAHv�AFn�AEdZAF  ADA�AAC�A?`BA=K�A;VA;K�A;�A<A;dZA:r�A9hsA933A9G�A9�-A9+A7�hA7\)A6��A6-A5�^A5K�A4��A3A37LA2v�A2$�A2bA2VA1�A1��A0��A/�TA.jA-�FA-p�A-7LA,JA+��A+33A*��A*��A*�A*�9A*I�A(�A(��A'�7A&ZA%�FA%?}A$�/A$�RA$jA$M�A$ �A#�mA#�A#|�A#dZA#G�A#�A"�A"9XA!�mA!��A!dZA!G�A!%A �A!+A!�wA!�A ��A (�A �9A ~�A  �A�A�A�-AĜA-AffA-A�-AVA^5A(�A|�AoA�!A^5A�A��AVA�uAVA1A�7A7LA+A�A�/A��AM�A{A��A"�A��A�HA�jAz�A�A5?AE�A1'A-A��AAC�A��AjA1'A�A��AhsA"�A;dA&�AĜAQ�A1Al�A%AbNA��A
��A
�A	��A	p�A	XA	"�A	�A	VA�9AVAv�AA�AbA�
A��A`BA�A�`AĜA��AVA�A�wA�hAK�A��A��A�AI�A�AA"�A��A-A�mA�A�A ��A ȴA r�A M�A �@�ȴ@��@�Q�@��@�t�@���@�
=@�J@��@�z�@�\)@�v�@�M�@�`B@�@��@�r�@�"�@�C�@�
=@�v�@�E�@�-@���@�@��@�Z@��m@�l�@�~�@���@홚@�9@�1'@�t�@�X@�h@��@畁@睲@�33@���@�M�@�{@�X@���@�@�D@�Q�@�b@�K�@�+@�^5@���@��`@ߕ�@��@�o@�@޸R@�-@ݺ^@�Ĝ@�  @�C�@���@�n�@��T@�O�@�z�@��@�+@�E�@Չ7@ԃ@�b@�|�@�C�@��@�+@���@�$�@��@���@�Z@�+@�ȴ@�M�@͙�@�&�@�/@�%@̣�@���@�\)@ʸR@�V@ɉ7@�&�@ȓu@�I�@���@ǅ@�o@Ə\@�ff@�5?@Ų-@�Ĝ@�(�@î@�"�@�{@�?}@�Z@��m@��
@��w@�S�@���@�=q@���@�/@���@�Z@��
@�dZ@���@�=q@���@��@��`@��9@�A�@��
@�C�@��@��!@�M�@��@�O�@�A�@���@��@�^5@��^@�&�@��@��@���@�z�@��@�t�@��@���@�@�V@���@���@��u@�9X@��
@�t�@�"�@��H@�-@��T@��#@��7@���@���@�j@�b@�dZ@�o@��@��R@�n�@�J@���@��@�X@���@��u@�9X@���@���@�t�@��@��+@�{@�hs@�/@���@��@��@�t�@�S�@�o@�^5@��@���@��@��D@���@��@��@�C�@���@��!@�~�@�-@��@���@��@�`B@�7L@��/@��@�bN@� �@��w@�
=@��!@�v�@�^5@�J@���@�hs@�?}@�%@��@�z�@�j@�1@��P@��@���@���@�v�@�M�@�-@���@�`B@�/@�%@��/@��9@��u@��D@�z�@�I�@��@��P@�C�@�
=@��y@��!@�ff@��T@��h@��@�p�@�7L@��@��/@��D@�bN@�Q�@�1'@�1@�ƨ@��@��@�dZ@�C�@���@��!@���@�~�@�-@�@���@��-@��h@�x�@�X@�G�@�Ĝ@�r�@�Z@�(�@��m@��P@�K�@�"�@���@��@��R@���@��\@�v�@�ff@�E�@�$�@�@���@��-@���@�p�@�X@�O�@�&�@��@�V@���@�bN@�I�@�(�@��@�ƨ@���@�|�@�l�@�;d@�ȴ@�~�@�=q@�$�@���@�?}@��`@���@��@���@��@�(�@�w@~��@~E�@~{@}�T@}��@}O�@}�@|�j@|�@{S�@{C�@z�@z^5@y��@y�^@yG�@xr�@w��@wl�@w;d@w
=@vȴ@v��@vE�@u��@t��@t�j@t(�@s�F@s��@sdZ@so@r^5@rJ@p��@pr�@pA�@o�@o|�@o�@n��@nV@n@m��@m��@m�@m`B@l�/@l�D@l�@kdZ@k@j��@j�@iG�@h��@hA�@h1'@h �@g�;@g�P@g;d@f��@f��@fE�@f{@e�T@e�h@e�@e?}@d�D@d9X@d�@d1@cƨ@c33@b�@b��@b=q@a�#@a��@ahs@a%@`r�@`b@_��@_K�@_;d@^��@^�+@^5?@]�T@]p�@\�@\�@\9X@[�
@[��@[33@Z�H@Zn�@Y��@Yx�@X��@X�@XA�@X  @W��@W�@V�@V�+@V5?@U@U/@T��@T�j@T�@T(�@S��@Sƨ@S33@S@R�H@Rn�@Q�@Q�^@Qx�@P��@Pb@P  @O�;@O�@OK�@N��@N@M�-@M�@Mp�@MO�@M�@LI�@Kƨ@KdZ@J�@J~�@J=q@JJ@I��@Ix�@I%@H �@G�P@GK�@G+@Fv�@E@D��@D�@DZ@C��@C��@C33@B�H@B~�@A��@A�^@A��@AX@AG�@A&�@@��@@�9@@Q�@?��@?|�@?K�@>�@>�+@=�-@=O�@=/@<�/@<j@;�m@;��@;"�@:��@:~�@:-@9�7@9%@8��@8Q�@8  @7�@7�P@6ȴ@6��@6ff@6$�@5�h@5/@4��@4��@4z�@4Z@49X@3�
@3t�@2�@2=q@2=q@2=q@2-@2-@2�@1��@1��@1G�@1%@0��@0�@/��@/+@.�y@.�@.�R@.V@-�@-��@-�@,�/@,��@,�j@,��@,Z@,9X@,(�@+��@+�
@+t�@+C�@+"�@*�H@*��@*�\@*n�@*=q@)�#@)��@)��@)�7@)7L@(�`@(A�@'�;@'��@'�@&��@&V@&V@&V@&V@&ff@&ff@&V@&E�@%�T@%�-@%`B@$�@$Z@$(�@$�@#��@#�
@#�F@#t�@"�@"�\@"�\@"�\@"~�@"^5@"=q@"-@!��@!��@!hs@!7L@ ��@ �9@ �u@ r�@ b@�@��@l�@;d@��@�+@v�@V@5?@5?@5?@�@@��@�@O�@/@��@��@z�@Z@1@�
@t�@o@�H@��@��@��@�\@~�@^5@�@��@�7@G�@�@Ĝ@�u@�@r�@bN@A�@A�@1'@1'@ �@  @��@\)@K�@�y@ff@E�@E�@{@�@/@�j@I�@�@�@�@�@�m@��@S�@"�@��@n�@^5@=q@J@�^@�7@hs@hs@G�@�`@�9@Q�@b@�;@�w@�P@K�@�@��@��@��@��@��@�y@ȴ@�+@E�@$�@@�@�T@@�-@��@�@�/@�/@��@�j@�jA�^5A�\)A�^5A�bNA�bNA�`BA�^5A�\)A�^5A�bNA�bNA�`BA�^5A�^5A�`BA�bNA�bNA�`BA�^5A�^5A�bNA�bNA�`BA�^5A�^5A�`BA�bNA�bNA�`BA�^5A�bNA�dZA�bNA�`BA�^5A�^5A�bNA�dZA�dZA�`BA�`BA�bNA�dZA�ffA�dZA�bNA�bNA�dZA�ffA�ffA�bNA�bNA�dZA�ffA�hsA�ffA�dZA�bNA�ffA�hsA�ffA�dZA�bNA�dZA�hsA�hsA�ffA�dZA�dZA�ffA�hsA�hsA�ffA�dZA�dZA�ffA�hsA�hsA�hsA�dZA�hsA�jA�l�A�jA�ffA�ffA�hsA�jA�jA�hsA�ffA�jA�M�A���AԑhA�C�A�(�A��A�{A�{A�oA�JA�%A���A���A���A�  A���A��A��yA��#A��
A���A���A�AӸRAӴ9AӴ9AӴ9AӲ-AӴ9AӼjA�A���AӸRAӝ�Aӟ�Aӧ�AӸRAӸRAӮAӛ�AӉ7A�t�A�K�A��AҬA�I�A���A���AхA��TA�A�A�bNAΙ�A�
=A�A͕�A��/A��`A�&�Aɡ�Aȧ�AǬAǼjA���A�t�A�
=AƧ�A�dZA�bNA�S�A��A���A�VA��`A�r�A�ZA�?}A�-A�VA��A��`A��;A��/A��/A��HA��/A���A��jA��-A���A���A�n�A�A�A��A��mA��jA��PA�hsA�Q�A�A�A�5?A�(�A��A�A��A��
A���A�ĜA���A��wA��RA��9A��A���A���A���A��uA�S�A��7A�%A���A���A�x�A�M�A�C�A�?}A�"�A�A��A���A���A���A��\A�~�A�`BA�9XA�(�A��A�A��A�p�A�  A�A���A��DA�^5A�7LA�A��DA�=qA��A�A���A��+A��7A�x�A�`BA�A�A�bA��A�5?A��A�
=A��A���A��uA�r�A�hsA�ZA�?}A�+A�(�A��A�JA���A��A��;A�ƨA��wA��^A���A��PA��A�G�A���A�%A�1'A��A��A��+A���A��yA��-A��DA�dZA�=qA���A��A�`BA�/A���A��RA�p�A�"�A���A��A�I�A� �A��A��HA��
A���A���A�ƨA�ȴA�ȴA�ȴA�ȴA�A��A�z�A�I�A�$�A���A���A��hA�jA�VA�A�A��A��TA��A�t�A�C�A��A���A���A���A�l�A�K�A�9XA�33A�/A�/A�+A��A�%A��A��/A�A���A�v�A�7LA��A��A���A���A���A��wA��A���A��A�M�A��A��RA��A�9XA���A���A���A��#A���A��RA���A�|�A�O�A�+A���A�ĜA�v�A�?}A�(�A�bA���A���A�p�A��HA�I�A��A��FA�jA�JA���A�z�A�K�A�K�A�ZA�v�A�x�A�O�A� �A�%A��HA���A���A�t�A�Q�A�(�A�ȴA�dZA�E�A�&�A�5?A�9XA�=qA�;dA�1'A�1'A�5?A�M�A�ZA�E�A��A���A��uA�\)A��A��HA���A�K�A���A���A�\)A��A���A���A�ffA�1'A�1A��A���A��!A��7A�bNA�9XA�%A��^A�E�A���A��#A��!A�ffA��A��hA�ffA�E�A��A�ȴA�ZA��^A�\)A�JA��A�`BA�{A���A�t�A��TA�S�A��7A�VA��
A�r�A���A�p�A�VA�/A���A��A�33A���A�Q�A��jA�~�A�5?A���A���A�~�A�K�A�bA��mA��RA�n�A�$�A���A��A���A�33A��A��RA�r�A�/A��A��uA�&�A��^A�1A���A�\)A�oA��TA��jA���A�r�A�ZA�33A�bA�A��At�A&�A~��A~v�A~JA}��A}�FA}�A|ĜA{ƨAz��Ax�AxbNAw��Av��Av��Av�uAvr�Av5?Au�Aul�At�\As�-Ar�/Aq�TAq33Ap{Ao33An�AnZAn1Am�FAm�AmdZAmS�Am7LAmoAl�RAl=qAk�;Akl�Ak;dAj�jAj1'Ai�-Ai��Ai�7AidZAi7LAi�Ai�AioAiAh�Ah�9AhA�AgAf�9Ae�^Ae+Ad��Ad^5Ad  Ac��Ac7LAb�DAb  Aa��Aa��Aa�wAa�Aa�A`�yA_�A^v�A]�
A]t�A]"�A\�A\��A\9XA[��A[+AZ�+AZ$�AY��AYt�AX��AX^5AW��AWƨAW�AW
=AV5?AT�AT(�AS�
AS��AS\)AS�AR��ARZAQ�AQ��AQ&�AP��AP��AP~�APZAP�AO�;AO��AOdZAO"�AN��AM��AL��AL��AL�ALE�AL-AL-AL �AJz�AI�-AI�7AI�7AI�AI�AI�AI�AI�AI�AI|�AI|�AI�AI�AI�AI�AI�AIt�AIp�AIhsAIXAIG�AI7LAI�AI
=AH��AHȴAH��AHVAG�TAGG�AF�RAF��AFv�AF=qAF1'AF1'AF{AEx�AE33AE�AEAEl�AE�mAF5?AE�
AEAE�AF{AF �AE�AEoAD�!AD�+AD9XAC;dABAA�AA�
AAp�A@�A@��A@�\A@�A?ƨA?�PA?"�A>��A>n�A>(�A=�TA=��A=+A<�A<E�A;�A;7LA:��A:�A:�yA:�`A:�A;%A;"�A;C�A;l�A;��A;�FA;��A;�TA;�A<  A<{A<{A<bA<1A<1A<1A<A;�A;�
A;�FA;�PA;XA;"�A;33A;/A;�A:�A:��A:^5A:{A9�;A9�-A9�hA9|�A9dZA9S�A97LA9/A9+A9+A9/A933A9;dA9?}A9;dA9;dA9;dA9?}A9G�A9XA9p�A9��A9��A9ƨA9��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BCaBB�BB�BC-BCaBC-BB�BB�BB�BB�BCaBB�BB�BB�BB�BB�BB�BA�BA B	pB
@�B
K�B
P�B
S�B
XB
Z�B
f�B
q�B
�rB
�dB�BAUBN�B� B��B�B�7B�XB��B�fBtBBLdBx�By	B|�B}�B|�Bu�B]�B\�Bf�BdZB�oBx�BY�BD3B0�BB
��B
�<B
�IB
�B
g�B
NpB
/�B
�B	�>B	�QB	ҽB	��B	�'B	�1B	cB	m]B	l�B	[�B	N<B	@�B	;0B	@�B	5tB	)_B	?B	�B	�YB	��B	�%B	��B	��B	�-B	�B	�WB	֡B	�NB	��B	��B	ÖB	�>B	�cB	��B	�]B
�B
�B
-CB
DgB
NpB
AUB
A B
HB
H�B
MB
K�B
Q�B
T�B
T�B
V�B
Z�B
_B
h�B
p;B
|�B
|�B
}VB
� B
yrB
�B
��B
��B
��B
�B
�7B
��B
��B
�(B
��B
��B
��B
��B
y	B
z�B
yrB
{B
{B
yrB
xB
xlB
y�B
zDB
y�B
{B
{�B
{JB
|�B
~�B
�B
��B
�B
�B
��B
��B
�1B
�SB
��B
��B
��B
�'B
�\B
��B
��B
��B
�PB
��B
��B
�B
�bB
�B
�rB
��B
cB
~]B
z�B
z�B
y	B
y	B
wfB
v�B
tB
s�B
r�B
s�B
sB
sMB
tTB
w�B
v+B
w2B
uZB
u%B
s�B
tB
u�B
u�B
tB
rB
sB
w�B
wfB
w2B
v�B
x8B
v�B
t�B
poB
oiB
l�B
k�B
jB
h
B
h�B
j�B
m]B
jB
gB
e�B
_�B
Z�B
W
B
N�B
L�B
K)B
J#B
J#B
L0B
OB
R�B
PHB
N<B
S�B
XEB
W�B
ZB
Y�B
Y�B
YB
XyB
WsB
W
B
WsB
U2B
T�B
S�B
S�B
Q�B
PB
O�B
N<B
K�B
JXB
F?B
C-B
AUB
@�B
?�B
=<B
?B
?HB
>BB
<jB
<6B
:*B
6FB
2�B
.B
.B
.�B
1�B
33B
.�B
,qB
+�B
'RB
($B
&B
)_B
,�B
,qB
(�B
,�B
-�B
-B
,=B
-CB
,�B
.�B
-B
-CB
*�B
+kB
*�B
(�B
($B
'�B
$tB
'B
�B
%�B
'�B
 �B
$tB
'�B
&�B
&�B
%zB
%FB
$@B
#�B
#�B
#nB
#:B
!�B
!�B
"4B
!�B
�B
B
CB
�B
B
CB
�B
kB
�B
�B
�B
MB
FB
B
�B
�B
uB
�B
@B
uB
B
�B
�B
�B
:B
�B
�B
FB
B
4B
�B
�B
�B
.B
bB
�B
hB
�B
bB
�B
�B
�B
(B
.B
 B
�B
FB
MB
SB
�B
�B
kB
�B
CB
CB
�B
B
�B
�B
�B
�B
�B
�B
�B
xB
xB
�B
�B
�B
�B
IB
�B
IB
B
B
�B
�B
B
B
IB
B
OB
�B
B
OB
�B
�B
VB
�B
!B
VB
 �B
�B
�B
�B
�B
�B
 'B
 \B
�B
 �B
!�B
"�B
#nB
#�B
#:B
#:B
#B
"hB
"4B
"hB
$B
$tB
$@B
$tB
$B
#�B
#:B
#�B
$@B
$@B
$@B
$tB
$�B
%FB
%FB
%FB
%B
%zB
%FB
&B
&�B
&�B
'B
&�B
(�B
($B
(XB
(XB
(�B
(�B
(�B
($B
'�B
'�B
(�B
(�B
(�B
(�B
*0B
)�B
*0B
*0B
*�B
+6B
+6B
+kB
+�B
+�B
+�B
+�B
+�B
+6B
,qB
,B
,B
,B
-B
.B
-�B
-�B
-�B
.}B
/B
.�B
/B
/OB
/�B
/�B
/B
0!B
0!B
0�B
0�B
0�B
0�B
0�B
0�B
1�B
1�B
1�B
2-B
2-B
2aB
2aB
2-B
2-B
2�B
2�B
3hB
3�B
3hB
3hB
33B
2aB
3hB
33B
3hB
3�B
3�B
3�B
4B
4�B
4�B
4�B
4�B
5B
5�B
5�B
6B
6zB
6zB
7LB
7�B
7�B
7�B
7�B
7�B
8RB
8�B
8�B
8�B
8�B
8�B
:^B
:^B
:*B
:�B
;0B
<B
<B
<jB
<�B
<�B
=B
<�B
=B
=B
=<B
=<B
=qB
=�B
>BB
>B
>wB
>�B
>wB
>�B
?B
>�B
>�B
?�B
?�B
?�B
@B
?�B
?}B
?}B
?�B
?HB
@B
@B
@�B
AUB
A B
B'B
C�B
B�B
B�B
C-B
B�B
B�B
C�B
C�B
DgB
D3B
DgB
D�B
EB
D�B
E9B
EmB
FtB
F�B
F?B
GB
F�B
GB
GB
GzB
G�B
G�B
HB
HKB
H�B
H�B
H�B
H�B
IRB
I�B
I�B
JXB
J#B
JXB
JXB
J�B
K)B
J�B
LdB
K�B
L0B
LdB
LdB
LdB
LdB
MjB
MjB
MjB
M�B
M�B
M�B
NB
M�B
NB
N�B
NpB
NpB
OBB
O�B
PB
PHB
PB
O�B
P�B
P�B
P�B
P�B
QNB
QNB
QB
QB
QNB
QNB
Q�B
RTB
R B
R B
Q�B
R B
R�B
R�B
S&B
R�B
S�B
S[B
S&B
S�B
TaB
T,B
T�B
T�B
T�B
T�B
UgB
UgB
UgB
VB
V9B
V9B
V�B
W
B
V�B
V�B
WsB
WsB
XB
XB
XyB
X�B
X�B
X�B
YB
YB
Y�B
Y�B
Y�B
ZQB
Z�B
Z�B
Z�B
Z�B
[WB
Z�B
[WB
[�B
[�B
[�B
\]B
\�B
\]B
\]B
]�B
]�B
]dB
]dB
]�B
]�B
^�B
^�B
_B
_B
^�B
_B
^�B
`BB
`B
_�B
`�B
`�B
`�B
`�B
aB
`�B
a�B
b�B
b�B
b�B
b�B
c B
cTB
c�B
c�B
c�B
dZB
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
ffB
ffB
ffB
f�B
f�B
g�B
g�B
g�B
h
B
h>B
h�B
hsB
h�B
h�B
iB
iyB
jB
jKB
jB
j�B
kB
kQB
kQB
l�B
l"B
lWB
lWB
l�B
m)B
l�B
m]B
m�B
m�B
n/B
n�B
n�B
oiB
o�B
o�B
o�B
o�B
o�B
o�B
pB
poB
p�B
p�B
p�B
p�B
qAB
q�B
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
s�B
sMB
sB
sB
r�B
s�B
s�B
sMB
t�B
tTB
t�B
t�B
t�B
u%B
uZB
u%B
uZB
u�B
u�B
u�B
v+B
u�B
u�B
v+B
v`B
v�B
w2B
v�B
w2B
w2B
w2B
wfB
wfB
xB
xB
xlB
x�B
y>B
y>B
y>B
y>B
yrB
yrB
y�B
zDB
zxB
zxB
zxB
zxB
z�B
z�B
z�B
{B
{B
{B
{�B
{�B
{�B
{�B
{�B
|B
|B
|PB
|PB
|�B
|�B
|�B
|�B
}"B
}"B
|�B
|�B
}VB
}�B
}�B
}�B
}�B
}�B
}�B
~(B
~]B
~�B
~�B
~�B
.B
�B
�B
�B
�B
�B
�B
�B
� B
�4B
�4B
��B
��B
�;B
�oB
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
�B
��B
�uB
�GB
�{B
�{B
�GB
��B
�B
��B
�B
��B
��B
��B
��B
�SB
��B
�%B
�%B
�%B
��B
��B
��B
��B
�+B
��B
��B
��B
��B
��B
�1B
�fB
�B
�7B
�7B
�lB
��B
��B
�	B
�	B
�	B
�	B
�	B
�	B
��B
�=B
��B
��B
��B
�B
��B
�B
�DB
�B
�DB
�xB
�JB
�B
�JB
��B
��BB�BD�BC�BB�BB[BB�BCaBC�BC-BA�BA�BB�BD3BC�BC�BB[BB'BB�BD�BC�BB�BB[BB�BC�BC�BC-BB'BB�BC�BC�BB�BB'BB�BC�BC�BD3BB�BA�BB�BC�BC�BC�BB�BA�BB[BC�BC�BB�BB[BB�BC�BC�BCaBB'BA�BB�BCaBC�BB�BAUBB'BB�BD3BCaBA�BA�BB�BC�BC-BB[BA�BA�BC-BC�BCaBB'BA�BA�BB[BC-BB'BA B@�BA BB'BB[BAUB@OB@�B@OBA�BXB	f�B	�sB
�B
*�B
5�B
;dB
=�B
A�B
EmB
G�B
J#B
K�B
K)B
K^B
K^B
MB
OB
QB
Q�B
PHB
PHB
QNB
S�B
S�B
S�B
R�B
R�B
TaB
TaB
W�B
W�B
X�B
\)B
V9B
V9B
VmB
X�B
^�B
\�B
\]B
[#B
a|B
aB
jB
f�B
m�B
f2B
b�B
k�B
xB
uZB
~�B
|�B
o5B
h�B
h�B
�7B
�oB
�"B
�XB
��B
��B
��B
��BYB�BB
�B
rB_B�BGzBQ�BA�B5tB5B=�B<BD�BIRBNpBP�BQ�BQ�BS�B]�BqvBu�Bt�BtBt�Bv+BzDB�B�B��B�4B�.B��B�VB��B�JB��B��B�B�rB�	B�1B��B��B��B�SB�B��B�B~]BzxB~�B��B�	B�xB�B�B�B��B��B�DB��B�B�PB� B� B��B�"B��B�uB�@B��B��B��B�-B�IB�VB�xB�B�B��B�@B��B�?B�XB��B�BɺB�NB��B�cB�rB��BoB�)B�`B�sB�B�ZB�	B��B�8B��B�ZB��B�;B�B��B��B�B�B�B��B�BٴB�EB� B��B��B�B��B�JB{BrGBl�BQNB8�B5�B/�B(XB1'B&�B$tBIB�BeB�B�B�B�B�B �B"hB!�B(�B49BB[BK^BM6BP}BX�B_;Be�Bm)Bu%Bs�Bq�Bt�B}�B{BwfBtTBw�B|�B}"B|�B{BzBw2Bv�Bx�B{�B{JBxlBzDByrBy�BzBy�B|B{Bz�By�By�BzB��B�AB�By�ByrBzDB�B��B�B|�B�4B|�B{�B~(Bt�B|PBw�BuZBncB}VB��B��B�B��B�GB�B�B}VBu�Bv�Bp;Bl�Bf�BrBz�BdZBi�BYBU2BU2BV9BV�BPBE�BFtBM6B^�BjKBn�Bj�Bj�Bk�Bj�BjBh
Be�Bd�Bk�B[�BW?BR BW�Bd�Bm�BqBs�Bp�Bu%Bw�B}"B�B�lB��B�B�MB��B.Bz�B{Bs�BrBiDBc Bd&BY�BW�BS�BQBJ#BK�BG�BE�BA BA B>BBA�B@B/OB)_B*0B.�B.�B �B�B�B@BSB�BB�B
��B
��B
�iB
�cB
��B
��B
�B
��B
��B
��B
��B
��B
�B
��B
��B
��B
�+B
�7B
�!B
��B
��B
�rB
zDB
}VB
zB
r|B
o�B
m�B
m�B
b�B
aHB
aHB
ZB
XB
T,B
c�B
J�B
>�B
9�B
8B
4�B
4�B
4nB
)�B
/�B
+�B
B
B
B
�B
�B
MB
�B
 iB	��B	�xB	�B	��B	�B	��B	� B	�B	�B	��B	�B	�B	�B	�HB	�ZB	֡B	��B	�BB	�UB	��B	�jB	�RB	�6B	�LB	��B	�B	�9B	��B	�LB	��B	��B	��B	��B	�.B	�hB	�JB	��B	��B	��B	�YB	��B	�DB	�%B	�_B	}�B	|B	��B	}�B	x�B	n�B	p�B	oiB	n�B	lWB	j�B	j�B	jB	h�B	m]B	o�B	ncB	u�B	l�B	c B	]�B	]�B	Y�B	X�B	YB	`vB	U2B	K�B	J�B	I�B	I�B	H�B	O�B	Z�B	VB	C�B	B'B	=qB	;�B	=B	<�B	=<B	AUB	;�B	9�B	7�B	7�B	:�B	=�B	:*B	6B	8�B	=�B	OBB	MjB	>wB	9$B	7�B	6�B	6FB	7�B	6FB	49B	2�B	/�B	*0B	'B	$�B	'RB	'�B	+B	*�B	-CB	,�B	-�B	M�B	@�B	D�B	[WB	l�B	sB	r�B	y�B	��B	�B	�B	��B	��B	��B	��B	��B	�YB	�YB	�+B	��B	�%B	��B	��B	��B	�B	�SB	�SB	�SB	��B	��B	��B	�fB	�1B	�1B	��B	��B	�DB	��B	�FB	��B	��B	�hB	�B	��B	��B	�$B	��B	�:B	�4B	��B	��B	�\B	�B	�B	��B	� B	�jB	�dB	�WB	�B	�[B	��B	�B	��B	��B	ɺB	��B	�B	��B	�KB	�9B	��B	ٴB	͟B	��B	��B	��B	��B	�B	��B	��B	�B	�}B	��B	��B	�B	�B	��B	��B	�eB	��B	�jB	� B	��B	��B	�jB	�NB	��B	�sB	�DB	��B	�B	��B	�WB	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	�	B	��B	�]B
B	�(B	��B
AB
�B
�B
xB
(B
B
�B
�B
�B
IB
 �B
$tB
&LB
)�B
+6B
,�B
,�B
/�B
-wB
4�B
;�B
A B
E�B
M�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                 B=B<vB<vB<�B=B<�B<�B<�B<�B<�B=B<�B<�B<vB<vB<vB<AB;�B:�B	i�B
:5B
EDB
JcB
MAB
Q�B
TlB
`�B
k\B
�$B
�B�B;BHWBy�B�@B��B��B�
B�B�Bm�B�BFBrSBr�BvkBw�Bv7Bo�BW~BVxB`MB^B{!BrSBS�B=�B*<B�B
�B
��B
��B
}�B
a�B
H"B
)jB
eB	��B	�B	�oB	�8B	��B	��B	yB	gB	f�B	UrB	G�B	:�B	4�B	:jB	/&B	#B	8�B	{�B	�B	:B	�B	��B	�OB	��B	��B	�	B	�SB	� B	�mB	�aB	�HB	��B	�B	�B	�B
�B
pB
&�B
>B
H"B
;B
:�B
A�B
B�B
F�B
E�B
KiB
N�B
N|B
PSB
TlB
X�B
b�B
i�B
v7B
v7B
wB
y�B
s$B
y~B
�B
}bB
��B
��B
��B
�FB
��B
��B
�RB
�}B
�LB
|�B
r�B
t�B
s$B
t�B
t�B
s$B
q�B
rB
sYB
s�B
s�B
t�B
u�B
t�B
vkB
x�B
yIB
zOB
{�B
{�B
}bB
{�B
��B
�B
�EB
�LB
�6B
��B
�B
�pB
�LB
��B
�B
�eB
:B
��B
�B
��B
�$B
|�B
yB
xB
t�B
t�B
r�B
r�B
qB
pFB
m�B
m4B
l�B
m�B
l�B
l�B
nB
q�B
o�B
p�B
oB
n�B
m�B
m�B
o@B
ouB
m�B
k�B
l�B
qLB
qB
p�B
p{B
q�B
pFB
nnB
j!B
iB
f�B
e7B
c�B
a�B
b�B
deB
gB
d1B
`�B
_GB
YVB
T8B
P�B
H�B
FJB
D�B
C�B
C�B
E�B
H�B
L�B
I�B
G�B
MAB
Q�B
QZB
S�B
SfB
SfB
S1B
R+B
Q%B
P�B
Q%B
N�B
NGB
MAB
MAB
KiB
I�B
I]B
G�B
E�B
D
B
?�B
<�B
;B
:jB
9�B
6�B
8�B
8�B
7�B
6B
5�B
3�B
/�B
,�B
'�B
'�B
(dB
+vB
,�B
(�B
&#B
%QB
!B
!�B
�B
#B
&WB
&#B
"�B
&�B
'�B
&�B
%�B
&�B
&WB
(dB
&�B
&�B
$�B
%B
$�B
"sB
!�B
!9B
&B
 �B
jB
aB
!9B
BB
&B
!9B
 gB
 3B
,B
�B
�B
�B
UB
 B
�B
�B
�B
�B
HB
<B
�B
�B
�B
�B
�B
�B
B
�B
tB
nB
�B
�B
�B
�B
�B
'B
[B
�B
'B
�B
�B
OB
�B
�B
�B
aB
�B
�B

�B
OB
<B
�B
	�B

B
	�B
B

}B

B
kB
kB
kB
�B
	�B

�B
�B
�B
�B
B
�B
zB
B
XB
�B
�B
�B
�B
XB
^B
�B
�B
�B
XB
XB
*B
*B
�B
�B
�B
^B
�B
�B
�B
�B
�B
�B
dB
�B
�B
�B
�B
B
�B
�B
B
�B
�B
B
jB
�B
B
BB
�B
�B
<B
<B
�B
�B
B
pB
BB
}B
�B
 B
UB
�B
�B
�B
B
�B
B
�B
&B
�B
&B
�B
UB
�B
UB
�B
�B
�B
&B
�B
�B
�B
�B
�B
,B
�B
�B
 3B
 gB
 �B
 �B
"?B
!�B
"
B
"
B
"?B
"sB
"?B
!�B
!mB
!�B
"?B
"?B
"sB
"�B
#�B
#�B
#�B
#�B
$KB
$�B
$�B
%B
%QB
%�B
%�B
%QB
%QB
$�B
&#B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'^B
(/B
(�B
(�B
(�B
)B
)jB
)5B
(�B
)�B
)�B
*<B
*pB
*�B
*�B
*�B
*pB
+vB
+vB
+vB
+�B
+�B
,B
,B
+�B
+�B
,HB
,�B
-B
-NB
-B
-B
,�B
,B
-B
,�B
-B
-NB
-�B
-NB
-�B
.�B
.TB
.TB
.�B
.�B
/ZB
/ZB
/�B
0,B
0,B
0�B
12B
12B
12B
1�B
1�B
2B
28B
2mB
2�B
2�B
2�B
4B
4B
3�B
4yB
4�B
5�B
5�B
6B
6QB
6QB
6�B
6�B
6�B
6�B
6�B
6�B
7#B
7WB
7�B
7�B
8)B
8]B
8)B
8]B
8�B
8�B
8]B
9�B
9�B
9�B
9�B
9�B
9/B
9/B
9cB
8�B
9�B
9�B
:5B
;B
:�B
;�B
=|B
<�B
<vB
<�B
<vB
<vB
=�B
=HB
>B
=�B
>B
>NB
>�B
>�B
>�B
?B
@&B
@ZB
?�B
@�B
@ZB
@�B
@�B
A,B
A`B
A�B
A�B
A�B
BfB
B�B
B�B
B�B
CB
ClB
ClB
D
B
C�B
D
B
D
B
D>B
D�B
DsB
FB
E�B
E�B
FB
FB
FB
FB
GB
GB
GB
GQB
GQB
GQB
G�B
GQB
G�B
HWB
H"B
H"B
H�B
I�B
I�B
I�B
I�B
I�B
JcB
JcB
JcB
J�B
K B
K B
J�B
J�B
K B
K B
KiB
LB
K�B
K�B
K�B
K�B
LoB
LoB
L�B
L�B
MAB
MB
L�B
MuB
NB
M�B
N|B
NGB
NGB
N�B
OB
OB
OB
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q%B
Q%B
Q�B
Q�B
R+B
R`B
R�B
R`B
S1B
S1B
SfB
S�B
SfB
TB
TlB
TlB
T8B
T8B
U	B
T�B
U	B
U�B
U>B
U>B
VB
VDB
VB
VB
WJB
WJB
WB
WB
WJB
W~B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Z\B
Z�B
Z\B
Z\B
Z�B
Z�B
[cB
\4B
\4B
\4B
\4B
\�B
]B
]�B
]�B
]�B
^B
^AB
^uB
^uB
^�B
_GB
_GB
_{B
_{B
_�B
_�B
_�B
_�B
_�B
`B
`B
`B
`MB
`MB
a�B
a�B
a�B
a�B
a�B
bYB
b%B
b�B
b�B
b�B
c+B
c�B
c�B
d1B
d�B
d�B
eB
eB
f=B
e�B
f	B
f	B
frB
f�B
f�B
gB
gCB
gxB
g�B
hJB
h~B
iB
i�B
i�B
i�B
i�B
iPB
i�B
i�B
j!B
jVB
jVB
jVB
j�B
j�B
k\B
k\B
k\B
k\B
k�B
k�B
l.B
l�B
l�B
m4B
mhB
mhB
l�B
l�B
l�B
lbB
mhB
mhB
l�B
n:B
nB
n:B
nnB
n�B
n�B
oB
n�B
oB
o@B
ouB
o@B
o�B
ouB
o@B
o�B
pB
p�B
p�B
p�B
p�B
p�B
p�B
qB
qB
q�B
q�B
rB
r�B
r�B
r�B
r�B
r�B
s$B
s$B
sYB
s�B
t*B
t*B
t*B
t*B
t_B
t�B
t�B
t�B
u1B
u1B
ueB
ueB
u�B
u�B
u�B
u�B
u�B
vB
vB
v7B
vkB
v�B
v�B
v�B
v�B
v�B
v�B
wB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xCB
xwB
xCB
x�B
yIB
yIB
yIB
yIB
y~B
y~B
yIB
y�B
y�B
y�B
zOB
z�B
z�B
{!B
{UB
{UB
{UB
{UB
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|\B
|'B
|�B
}-B
}-B
|�B
}�B
}�B
~3B
~�B
:B
nB
:B
:B
B
nB
�B
�B
�B
�tB
��B
�tB
��B
��B
�zB
�zB
�zB
�zB
�zB
��B
�B
��B
��B
��B
�B
�RB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�XB
�XB
��B
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
�eB
��B<vB>NB=|B<AB<B<vB=B=|B<�B;�B;pB<vB=�B=�B=HB<B;�B<�B>NB=�B<vB<B<vB=|B=�B<�B;�B<AB=|B=�B<�B;�B<AB=HB=�B=�B<vB;�B<AB=|B=|B=HB<vB;�B<B=HB=�B<vB<B<AB=HB=|B=B;�B;pB<AB=B=|B<AB;B;�B<vB=�B=B;�B;pB<vB=HB<�B<B;pB;�B<�B=HB=B;�B;;B;;B<B<�B;�B:�B:jB:�B;�B<B;B:B:5B:B;�BQ�B	`�B	�%B
qB
$KB
/�B
5B
7WB
;pB
?B
A�B
C�B
EDB
D�B
EB
EB
F�B
H�B
J�B
KiB
I�B
I�B
K B
MuB
MuB
MuB
LoB
LoB
NB
NB
Q�B
Q�B
R�B
U�B
O�B
O�B
PB
R`B
XPB
VxB
VB
T�B
[.B
Z�B
d1B
`�B
gCB
_�B
\�B
e7B
q�B
oB
xCB
v7B
h�B
bYB
bYB
��B
�!B
��B
�
B
�RB
�KB
�[B
�BB	wB�BXB$BB9BA,BK5B;pB/&B.�B7WB5�B>�BCBH"BJcBKiBK�BMABWJBk(Bo@BnnBm�BnnBo�Bs�B~�B��B��B��B��B�qB�B�eB��B�^B�^B��B�$B��B��B�@BnB:BB~�B:B|�BxBt*BxCB��B��B�*B��B��B��B�XB��B��B��B��B�B��B��B�6B��B�UB�'B��B��B�3B�EB��B��B�B�*B��B��B��B��B��B��B�
B�jB��B�lB� B�CB�B�$B�YB�!B��B�B�%B�B�B�B�{B��B�@B�B�B��B�=B�B�B�=B�1B�eBۗB��B�fB��B��B�|BҔB��B��B��Bt�Bk�Bf�BK B2mB/�B)jB"
B*�B �B&B�BRBB3B�B�B�BLBwBBHB"sB-�B<BEBF�BJ/BR�BX�B_GBf�Bn�Bm�Bk�Bn�BwqBt�BqBnBq�Bv7Bv�BvkBu1Bs�Bp�BpFBr�BueBt�BrBs�Bs$Bs�Bs�Bs�Bu�Bu1Bt�Bs�BsYBs�Bz�B{�Bz�BsYBs$Bs�ByIB|�By~BvkBy�Bv�BueBw�BnnBvBqLBoBhBwB{�B~3B|�B~hB|�By~ByIBwBo�Bp�Bi�Bf�B`�Bk�Bt�B^Bc�BS1BN�BN�BO�BPSBI�B?�B@&BF�BXPBc�Bh~BdeBdeBelBdeBd1Ba�B_�B^ABe7BUrBP�BK�BQ�B^uBgCBj�BmhBj�Bn�Bq�Bv�B��B�B��B}�B}�BzOBx�Bt_Bu1Bm4Bk�Bb�B\�B]�BS�BQZBMABJ�BC�BEyBA�B?�B:�B:�B7�B;pB9�B)B#B#�B(dB(�BwBRB9B�BBRB�B
�UB
�FB
�7B
�B
�B
��B
�rB
�cB
�{B
ЈB
��B
�>B
��B
��B
�BB
��B
��B
��B
��B
��B
�wB
�9B
�$B
s�B
wB
s�B
l.B
i�B
gxB
gxB
\�B
Z�B
Z�B
S�B
Q�B
M�B
]oB
D�B
8]B
3sB
1�B
.�B
.TB
. B
#yB
)jB
%�B
�B
�B
�B
�B	��B	��B	��B	�B	�qB	�*B	�B	�B	�4B	�nB	�B	�hB	�1B	�{B	�\B	�oB	��B	��B	�B	�SB	̤B	��B	�B	�cB	�B	�B	��B	��B	�EB	��B	��B	�>B	��B	�gB	��B	�BB	�zB	��B	�B	��B	�XB	�zB	�B	�B	�@B	��B	�B	�B	wqB	u�B	zOB	w�B	rSB	h~B	jVB	iB	h~B	f	B	deB	deB	d1B	b�B	gB	iPB	hB	o@B	frB	\�B	W~B	WJB	SfB	R`B	R�B	Z(B	N�B	EDB	D�B	C8B	C�B	B�B	I�B	T8B	O�B	=HB	;�B	7#B	5KB	6�B	6�B	6�B	;B	5KB	3>B	12B	1�B	4yB	7�B	3�B	/�B	2�B	7�B	H�B	GB	8)B	2�B	12B	0`B	/�B	1�B	/�B	-�B	,HB	)5B	#�B	 �B	[B	!B	!mB	$�B	$KB	&�B	&�B	'�B	GQB	:jB	>NB	U	B	frB	l�B	l�B	sYB	��B	}�B	��B	�@B	nB	:B	�B	nB	�B	�B	��B	�@B	�B	:B	~�B	~hB	~�B	B	B	B	�tB	�zB	nB	�B	��B	��B	�FB	�XB	��B	�nB	��B	��B	��B	�B	��B	��B	�wB	��B	�[B	��B	��B	�UB	�UB	�B	��B	ǹB	��B	��B	�B	�B	�	B	عB	�B	̤B	϶B	�B	ͪB	�lB	ǅB	�AB	�{B	��B	��B	ݣB	�fB	�QB	B	ȋB	��B	��B	��B	�8B	��B	��B	�/B	��B	��B	��B	��B	��B	�3B	�B	�vB	�B	��B	�sB	�|B	�B	� B	�B	�%B	��B	�B	�lB	�rB	�	B	�rB	�B	�B	��B	�B	�B	�@B	�LB	�bB	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B
 �B
*B
�B
�B
EB
LB
�B
�B
�B
&B
�B
#�B
$�B
&WB
&WB
)5B
')B
.�B
5KB
:�B
?�B
G�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223244                            20230426223244AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622324420230426223244  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622324420230426223244QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622324420230426223244QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               