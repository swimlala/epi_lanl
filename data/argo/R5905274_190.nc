CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:35Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230426223235  20230426223235  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @���[D@���[D11  @��   @��   @/����i@/����i�dM�P]��dM�P]�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AB  ?���@�\@@  @�  @�G�@�G�@�G�A ��AG�A   A*�HA?\)A`��A�Q�A�  A��A��A�  A�  A�  A�  A��B  B  B  B   B((�B0(�B8  B?�
BH  BP(�BX(�B`  Bh  Bp  Bw�
B�B�{B�{B�{B�{B�  B�  B�  B��B�{B�{B�  B�  B��B�  B�  B�  B�  B��B�{B�  B�  B�  B��
B�  B�{B�(�B�(�B�{B�  B�  B��B��C
=C{C
=C  C
33C��C  C  C
=C
=C
=C��C  C  C  C   C"
=C$  C%�C'��C*  C+��C-��C0{C2
=C3��C5��C8  C:  C<  C=��C?�CB  CD  CF  CH
=CJ
=CL  CM��CO��CR  CT
=CV
=CX
=CZ  C[��C]��C`
=Cb
=Cd  Ce��Cg��Ci��Ck��Cm��Cp  Cq��Ct
=Cv{Cx  Cz  C{�C}�C��C�C�C�  C�  C�  C�C�C�C�C�C���C���C�C�C�C�C���C��C���C���C���C���C���C��C���C���C�  C�  C�C�C�  C�  C�C�  C�  C�
=C�
=C�
=C�\C�
=C�C�C�
=C�
=C�
=C�C�
=C�
=C�
=C�  C���C���C���C���C�  C�  C���C���C���C���C���C���C�  C�  C�C�C���C�C�C���C�
=C�  C���C�  C���C�  C���C���C�  C�  C���C�  C���C�C�  C�  C�C�C�  C�C�  C�  C���C���C���C���C���C���C���C�  C���C�  C�C�C���C���C���C�  C�  C���C���C���C���C���C�  C�  C�
=C�  C�  C���C���C�  C�  C���C�  C�  C�
=D �D � DD��D�qD� D  D}qD��Dz�D  D��D  D� D�qDxRD�RDxRD��D	}qD	�qD
� D  D��DD�D�D�D�qDz�D  D}qD�qDz�D  D��D�qD��D�qD��DD��D�D��D�D}qD��D}qD  D��D  D}qD�D�DD��D  Dz�D��Dz�D  D� D�qD��D �D �D!D!��D"�D"��D"��D#}qD$�D$�D%�D%��D&  D&}qD&�qD'}qD'��D(}qD)  D)� D*�D*��D*�qD+�D,D,� D,�qD-z�D.D.�D/D/��D/�qD0� D1  D1xRD2  D2�D2��D3z�D4  D4��D5  D5z�D6�D6��D6�RD7}qD7�qD8��D8�qD9z�D9�qD:z�D:�qD;xRD<�D<��D=�D=�D=�qD>��D?  D?� D?�RD@��DA�DA� DA��DB��DC�DC�=DC�qDD� DD�qDEz�DE�qDF}qDF��DG� DH�DH�DIDI��DJ  DJ� DJ�qDKz�DK��DL}qDMDM� DM�qDN}qDN��DOz�DO�qDP� DQ  DQ� DR  DR}qDS�DS�DT�DT��DU�DU� DU�qDV��DW�DW��DX�DX��DY  DY}qDZ  DZ��D[  D[��D\  D\}qD]  D]}qD]�qD^}qD^�qD_}qD`  D`��D`�qDa�Da�qDb� Db�qDc}qDd�Dd}qDe  De��De�qDf� Df�qDg� Dg�qDh� Di�Di� Dj  Djz�DkDk� Dk�qDl��Dm�Dm��Dn  Dn� Do  Do� Dp  Dp}qDq  Dq� Dr  Dr� Ds�Ds� Ds�qDt� Du  Du}qDv  Dv��Dw  Dw� Dx�Dx� Dx�qDy� Dz  Dzz�D{  D{z�D|  D|z�D}�D}� D~  D~��D~�qD}qD�  D�>�D���D�� D�HD�@ D��HD�� D���D�>�D�~�D���D�  D�>�D�� D�� D���D�>�D�� D���D�  D�@ D��HD��HD�HD�@ D�� D���D���D�AHD�� D�� D�HD�>�D�� D�� D�  D�>�D��HD��HD��D�>�D�� D�� D��D�@ D�� D��HD��D�@ D�� D�� D���D�AHD�}qD�� D�  D�@ D��HD�� D���D�@ D��HD��HD���D�@ D�� D�� D�  D�AHD��HD�� D���D�>�D�� D��HD�  D�AHD��HD�� D�  D�>�D�� D��HD�  D�@ D�� D��HD�  D�>�D�� D�� D��qD�>�D�~�D�� D�HD�>�D�� D��HD���D�AHD��HD��HD�HD�@ D�~�D���D�  D�AHD�� D�� D��D�>�D�� D��qD�  D�>�D�}qD���D���D�=qD��HD�� D���D�>�D�~�D�� D�  D�@ D�}qD��qD�  D�@ D�� D�D��qD�@ D��HD��HD�HD�@ D�~�D���D�  D�@ D��HD�� D�  D�AHD��HD��HD�  D�@ D�~�D�� D�HD�@ D��HD��HD�HD�B�D�� D��qD�  D�>�D���D��HD��D�AHD��HD���D�  D�@ D�� D���D�  D�B�D�~�D�� D�HD�=qD��HD��HD���D�AHD���D�� D�  D�B�D��HD�� D�HD�AHD�� D�� D�  D�AHD�� D��HD�  D�B�D�|)D�� D��D�=qD�~�D�� D�HD�AHD�}qD���D�HD�=qD�|)D���D��qD�=qD��HD���D�  D�@ D�|)D�� D�  D�=qD�|)D��qD��qD�@ D��HD��HD��D�@ D�~�D�� D��D�>�D�~�D��HD�  D�@ D��HD�� D��qD�=qD�� D�D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D��HD�� D���D�@ D�~�D�� D���D�>�D D¾�D�  D�AHDÀ D��HD�HD�@ DāHD�D��D�@ DŁHD�D���D�@ D�~�D�� D���D�>�DǁHD��HD��)D�=qD�}qDȾ�D���D�>�Dɀ D��HD�HD�>�D�}qDʾ�D���D�>�DˁHD��HD�HD�AHD�~�D̽qD��qD�@ D̀ D;�D�  D�B�D΂�D��HD���D�@ Dς�D��HD�HD�AHDЁHD��HD���D�=qDр D�� D�  D�@ DҀ D��HD�  D�AHDӁHD�� D��qD�>�DԁHD��HD�  D�@ D�~�Dվ�D���D�>�Dր D־�D��qD�>�DׁHD���D�  D�@ D؂�D���D��D�>�D�~�D��HD��qD�@ DځHD�� D���D�>�D�}qD۾�D��D�B�D܁HDܾ�D��qD�@ D݁HD��HD�HD�@ Dހ D�� D�  D�>�D�}qD߽qD�  D�B�D��HD��HD�  D�>�D� D�� D�HD�@ D�HD�� D�  D�@ D�HD�qD�  D�AHD�HD��HD���D�=qD�~�D�� D���D�@ D�HD��HD�HD�@ D� D�� D�  D�AHD�HD�� D���D�=qD� D�D��D�AHD� D�qD��)D�>�D낏D��HD�  D�B�D�HD��HD�HD�>�D�~�D��HD��D�@ D� D��HD�  D�AHD�HD��HD���D�@ D���D�D���D�B�D�HD�� D�HD�@ D�HD��HD���D�@ D�HD��HD�  D�@ D� D�� D�HD�B�D�~�D��qD�HD�C�D���D�� D�  D�>�D�}qD���D���D�@ D��HD���D��qD�@ D���D���D�  D�>�D�ff?k�?u?���?�{?���?�G�?�@�@�@�R@+�@:�H@G�@Tz�@aG�@s33@�  @��@�{@�
=@�p�@��@�\)@�Q�@��R@�ff@�{@�@�p�@��@�@�33@�Q�A   A�
A�A(�A\)A�
A
=A�A ��A$z�A(��A,��A0��A4z�A8Q�A:�HA?\)AA�AFffAJ=qAN�RAR�\AVffAZ�HA^{Ac33Ag
=Ak�Ap  As�
Ax��A|��A�Q�A��\A�z�A�{A�  A���A��
A�A��A���A�(�A��RA���A��\A���A��RA�  A��A��
A�A�  A��A��
A�A�  A��A��
A�{A���A��HA���A�\)A���A\A�z�A�{A�Q�A�=qA��
A�{A���Aҏ\A��AָRA�  A��A�(�A�A�  A�\A���A�RA���A��HA�z�A�{A�  A�=qA��
A�{A�Q�A��HA���A��RB (�B ��B�B�HB  B��B{B�HB  B	G�B
=qB\)Bz�Bp�BffB\)Bz�Bp�BffB�B(�B�B{B
=B(�Bp�B�\B�B��BB�RB�B z�B!p�B"ffB#\)B$��B%�B'
=B((�B)�B*=qB*�HB+�B,��B-�B.�HB0  B1�B2�\B3�B4z�B5B6�\B7\)B8Q�B9p�B:ffB;\)B<z�B=�B?
=B@  BAG�BB=qBC\)BD  BD��BF{BG
=BH  BI�BJ{BK�BL��BMBN�RBO�
BP��BQ��BR�RBS�BT��BU�BW33BXQ�BYp�BZ�\B[\)B\��B]��B^ffB_\)B`z�Bap�Bb�\Bc�Bd��Be�Bg33Bh(�Bi�BjffBk\)Blz�Bm�Bn{Bo33BpQ�Bqp�Br�\Bs�
Bu�Bv=qBw33Bxz�Byp�BzffB{\)B|Q�B}G�B~ffB�  B�z�B�
=B��B�{B�z�B�
=B�p�B��B�z�B���B�p�B�  B�z�B�
=B��B�Q�B��HB�p�B�  B��\B��B�p�B�  B��\B��B���B�Q�B���B��B�{B���B��B��B�{B���B��B�B�z�B��B��B�=qB��\B�33B�B�Q�B��B��B�Q�B���B�33B�B�Q�B���B���B�(�B���B�G�B��B�Q�B��RB�\)B�{B��RB�G�B��B�Q�B���B�\)B��B���B�\)B��B�ffB��HB��B�  B��HB�p�B�  B�Q�B��HB��B�=qB���B�G�B��B�Q�B��HB��B�=qB���B��B��B�=qB���B��B�{B�z�B�
=B���B�ffB�
=B��B��B�z�B��B��
B���B�G�B�B�=qB���B�p�B�(�B��HB��B��B\B�
=B��Bď\B�33BŮB�(�BƸRBǙ�B�(�Bȣ�B�33B��
B�ffB�33B�B�{B̸RB�33B�  BΣ�B�G�BϮB�=qB���Bљ�B�=qB��HB�p�B��B�z�B�33B��B֏\B���BׅB�(�B���Bٙ�B�Q�BڸRB�G�B��B܏\B�p�B�(�Bޏ\B�G�B��B�RB�p�B��
B�z�B�33B�  B��B��B�B�z�B�G�B�  B�ffB�
=B�B�z�B�33B뙚B�=qB��HB�B�ffB��HB�p�B�=qB���B�B�=qB�RB�\)B�=qB���B�p�B�{B���B���B�  B��RB���B�(�B��RB�p�B�Q�B���B��B�Q�B�33B���C (�C ��C ��C33C�C  CQ�C�\C��C\)C��C��CffCC��C\)CC
=CQ�C��C�CffCC(�C\)C�C	{C	Q�C	�C	�
C
{C
(�C
G�C
��C
�RC
�RC
�HC  C
=C
=C(�C=qCQ�CG�Cp�C�Cp�C�C�C�C�RC�HC��C�C��C(�C=qC33C=qCp�Cz�Cz�C��C�RC�C�RC��C  C  C33CQ�CG�Cp�C�\C�C��C�
C��C�C(�C(�C=qCp�C�\C�C��C�
C�
C�C(�C=qC=qCp�C��C�\C��C�C�C�C=qC=qCp�C��C��C�RC�C�C�CG�C=qCz�C��C��C�
C��C��C(�CQ�CQ�Cz�C�C�C��C  C��C�CQ�CG�CffC��C��C�C�HC�HC
=C33C(�C\)Cp�Cp�C��C�C�C�C�C�C=qC=qCffCp�C�C�RCC��C
=C{C�CQ�C\)Cp�C��C�RC�RC�C��C  C33C(�C=qCp�CffC��C��C�C�
C�
C�HC�C�CG�CffC\)C��C��CC�C�C�C�C33CffCp�C��C�RCC�C�C
=C=qC=qCp�Cp�C�\CCC��C��C{CG�C=qCz�Cz�C��C�RC�
C  C  C33C33CffCz�C�CCC  C  C{C\)C\)C�\C��C��C�HC��C=qCG�Cz�C�CC�HC��C=qCG�C�C��C�
C��C {C \)C ffC �C C!
=C!�C!\)C!�C!��C!�C!��C"G�C"\)C"��C"�C#  C#{C#=qC#�C#��C#�C#��C$G�C$ffC$�C$C%{C%33C%ffC%�C%C&�C&(�C&�C&��C&��C'{C'ffC'�C'�
C'��C(G�C(ffC(�RC(�HC)33C)Q�C)��C)C*{C*33C*�\C*�C*��C+(�C+p�C+��C+�
C,{C,Q�C,�C,�RC-  C-{C-p�C-�C-�HC-�C.G�C.\)C.�C.��C/�C/G�C/��C/�RC0
=C033C0�C0��C1  C1(�C1ffC1��C1��C2(�C2=qC2��C2�RC3{C3(�C3�\C3�C4  C4�C4z�C4�\C4�C5
=C5\)C5�C5�
C5��C6G�C6p�C6C6�C733C7\)C7�RC7��C833C8G�C8��C8C9{C9=qC9�C9C9�HC:=qC:\)C:�C:C;{C;33C;�C;��C;��C<{C<ffC<p�C<��C<�C=33C=Q�C=�\C=��C=�C>=qC>Q�C>��C>C?
=C?�C?p�C?�\C?�
C?�C@G�C@Q�C@��C@CA
=CA�CAz�CA�\CA��CB  CB(�CBp�CB�CB�
CB�CC=qCC\)CC��CC�RCD{CD(�CDffCD��CDCE
=CE(�CEp�CE�\CE�
CE��CF=qCFQ�CF��CFCF��CG=qCG\)CG�CGG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411141114114114111411141114114111411411411411411141141114114114114114114114114114114114114114114114114114114141141111111111111141111411411114114114141141411111114114141141411414111141411114141141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                          ?���@�\@@  @�  @�G�@�G�@�G�A ��AG�A   A*�HA?\)A`��A�Q�A�  A��A��A�  A�  A�  A�  A��B  B  B  B   B((�B0(�B8  B?�
BH  BP(�BX(�B`  Bh  Bp  Bw�
B�B�{B�{B�{B�{B�  B�  B�  B��B�{B�{B�  B�  B��B�  B�  B�  B�  B��B�{B�  B�  B�  B��
B�  B�{B�(�B�(�B�{B�  B�  B��B��C
=C{C
=C  C
33C��C  C  C
=C
=C
=C��C  C  C  C   C"
=C$  C%�C'��C*  C+��C-��C0{C2
=C3��C5��C8  C:  C<  C=��C?�CB  CD  CF  CH
=CJ
=CL  CM��CO��CR  CT
=CV
=CX
=CZ  C[��C]��C`
=Cb
=Cd  Ce��Cg��Ci��Ck��Cm��Cp  Cq��Ct
=Cv{Cx  Cz  C{�C}�C��C�C�C�  C�  C�  C�C�C�C�C�C���C���C�C�C�C�C���C��C���C���C���C���C���C��C���C���C�  C�  C�C�C�  C�  C�C�  C�  C�
=C�
=C�
=C�\C�
=C�C�C�
=C�
=C�
=C�C�
=C�
=C�
=C�  C���C���C���C���C�  C�  C���C���C���C���C���C���C�  C�  C�C�C���C�C�C���C�
=C�  C���C�  C���C�  C���C���C�  C�  C���C�  C���C�C�  C�  C�C�C�  C�C�  C�  C���C���C���C���C���C���C���C�  C���C�  C�C�C���C���C���C�  C�  C���C���C���C���C���C�  C�  C�
=C�  C�  C���C���C�  C�  C���C�  C�  C�
=D �D � DD��D�qD� D  D}qD��Dz�D  D��D  D� D�qDxRD�RDxRD��D	}qD	�qD
� D  D��DD�D�D�D�qDz�D  D}qD�qDz�D  D��D�qD��D�qD��DD��D�D��D�D}qD��D}qD  D��D  D}qD�D�DD��D  Dz�D��Dz�D  D� D�qD��D �D �D!D!��D"�D"��D"��D#}qD$�D$�D%�D%��D&  D&}qD&�qD'}qD'��D(}qD)  D)� D*�D*��D*�qD+�D,D,� D,�qD-z�D.D.�D/D/��D/�qD0� D1  D1xRD2  D2�D2��D3z�D4  D4��D5  D5z�D6�D6��D6�RD7}qD7�qD8��D8�qD9z�D9�qD:z�D:�qD;xRD<�D<��D=�D=�D=�qD>��D?  D?� D?�RD@��DA�DA� DA��DB��DC�DC�=DC�qDD� DD�qDEz�DE�qDF}qDF��DG� DH�DH�DIDI��DJ  DJ� DJ�qDKz�DK��DL}qDMDM� DM�qDN}qDN��DOz�DO�qDP� DQ  DQ� DR  DR}qDS�DS�DT�DT��DU�DU� DU�qDV��DW�DW��DX�DX��DY  DY}qDZ  DZ��D[  D[��D\  D\}qD]  D]}qD]�qD^}qD^�qD_}qD`  D`��D`�qDa�Da�qDb� Db�qDc}qDd�Dd}qDe  De��De�qDf� Df�qDg� Dg�qDh� Di�Di� Dj  Djz�DkDk� Dk�qDl��Dm�Dm��Dn  Dn� Do  Do� Dp  Dp}qDq  Dq� Dr  Dr� Ds�Ds� Ds�qDt� Du  Du}qDv  Dv��Dw  Dw� Dx�Dx� Dx�qDy� Dz  Dzz�D{  D{z�D|  D|z�D}�D}� D~  D~��D~�qD}qD�  D�>�D���D�� D�HD�@ D��HD�� D���D�>�D�~�D���D�  D�>�D�� D�� D���D�>�D�� D���D�  D�@ D��HD��HD�HD�@ D�� D���D���D�AHD�� D�� D�HD�>�D�� D�� D�  D�>�D��HD��HD��D�>�D�� D�� D��D�@ D�� D��HD��D�@ D�� D�� D���D�AHD�}qD�� D�  D�@ D��HD�� D���D�@ D��HD��HD���D�@ D�� D�� D�  D�AHD��HD�� D���D�>�D�� D��HD�  D�AHD��HD�� D�  D�>�D�� D��HD�  D�@ D�� D��HD�  D�>�D�� D�� D��qD�>�D�~�D�� D�HD�>�D�� D��HD���D�AHD��HD��HD�HD�@ D�~�D���D�  D�AHD�� D�� D��D�>�D�� D��qD�  D�>�D�}qD���D���D�=qD��HD�� D���D�>�D�~�D�� D�  D�@ D�}qD��qD�  D�@ D�� D�D��qD�@ D��HD��HD�HD�@ D�~�D���D�  D�@ D��HD�� D�  D�AHD��HD��HD�  D�@ D�~�D�� D�HD�@ D��HD��HD�HD�B�D�� D��qD�  D�>�D���D��HD��D�AHD��HD���D�  D�@ D�� D���D�  D�B�D�~�D�� D�HD�=qD��HD��HD���D�AHD���D�� D�  D�B�D��HD�� D�HD�AHD�� D�� D�  D�AHD�� D��HD�  D�B�D�|)D�� D��D�=qD�~�D�� D�HD�AHD�}qD���D�HD�=qD�|)D���D��qD�=qD��HD���D�  D�@ D�|)D�� D�  D�=qD�|)D��qD��qD�@ D��HD��HD��D�@ D�~�D�� D��D�>�D�~�D��HD�  D�@ D��HD�� D��qD�=qD�� D�D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D��HD�� D���D�@ D�~�D�� D���D�>�D D¾�D�  D�AHDÀ D��HD�HD�@ DāHD�D��D�@ DŁHD�D���D�@ D�~�D�� D���D�>�DǁHD��HD��)D�=qD�}qDȾ�D���D�>�Dɀ D��HD�HD�>�D�}qDʾ�D���D�>�DˁHD��HD�HD�AHD�~�D̽qD��qD�@ D̀ D;�D�  D�B�D΂�D��HD���D�@ Dς�D��HD�HD�AHDЁHD��HD���D�=qDр D�� D�  D�@ DҀ D��HD�  D�AHDӁHD�� D��qD�>�DԁHD��HD�  D�@ D�~�Dվ�D���D�>�Dր D־�D��qD�>�DׁHD���D�  D�@ D؂�D���D��D�>�D�~�D��HD��qD�@ DځHD�� D���D�>�D�}qD۾�D��D�B�D܁HDܾ�D��qD�@ D݁HD��HD�HD�@ Dހ D�� D�  D�>�D�}qD߽qD�  D�B�D��HD��HD�  D�>�D� D�� D�HD�@ D�HD�� D�  D�@ D�HD�qD�  D�AHD�HD��HD���D�=qD�~�D�� D���D�@ D�HD��HD�HD�@ D� D�� D�  D�AHD�HD�� D���D�=qD� D�D��D�AHD� D�qD��)D�>�D낏D��HD�  D�B�D�HD��HD�HD�>�D�~�D��HD��D�@ D� D��HD�  D�AHD�HD��HD���D�@ D���D�D���D�B�D�HD�� D�HD�@ D�HD��HD���D�@ D�HD��HD�  D�@ D� D�� D�HD�B�D�~�D��qD�HD�C�D���D�� D�  D�>�D�}qD���D���D�@ D��HD���D��qD�@ D���D���D�  D�>�D�ff?k�?u?���?�{?���?�G�?�@�@�@�R@+�@:�H@G�@Tz�@aG�@s33@�  @��@�{@�
=@�p�@��@�\)@�Q�@��R@�ff@�{@�@�p�@��@�@�33@�Q�A   A�
A�A(�A\)A�
A
=A�A ��A$z�A(��A,��A0��A4z�A8Q�A:�HA?\)AA�AFffAJ=qAN�RAR�\AVffAZ�HA^{Ac33Ag
=Ak�Ap  As�
Ax��A|��A�Q�A��\A�z�A�{A�  A���A��
A�A��A���A�(�A��RA���A��\A���A��RA�  A��A��
A�A�  A��A��
A�A�  A��A��
A�{A���A��HA���A�\)A���A\A�z�A�{A�Q�A�=qA��
A�{A���Aҏ\A��AָRA�  A��A�(�A�A�  A�\A���A�RA���A��HA�z�A�{A�  A�=qA��
A�{A�Q�A��HA���A��RB (�B ��B�B�HB  B��B{B�HB  B	G�B
=qB\)Bz�Bp�BffB\)Bz�Bp�BffB�B(�B�B{B
=B(�Bp�B�\B�B��BB�RB�B z�B!p�B"ffB#\)B$��B%�B'
=B((�B)�B*=qB*�HB+�B,��B-�B.�HB0  B1�B2�\B3�B4z�B5B6�\B7\)B8Q�B9p�B:ffB;\)B<z�B=�B?
=B@  BAG�BB=qBC\)BD  BD��BF{BG
=BH  BI�BJ{BK�BL��BMBN�RBO�
BP��BQ��BR�RBS�BT��BU�BW33BXQ�BYp�BZ�\B[\)B\��B]��B^ffB_\)B`z�Bap�Bb�\Bc�Bd��Be�Bg33Bh(�Bi�BjffBk\)Blz�Bm�Bn{Bo33BpQ�Bqp�Br�\Bs�
Bu�Bv=qBw33Bxz�Byp�BzffB{\)B|Q�B}G�B~ffB�  B�z�B�
=B��B�{B�z�B�
=B�p�B��B�z�B���B�p�B�  B�z�B�
=B��B�Q�B��HB�p�B�  B��\B��B�p�B�  B��\B��B���B�Q�B���B��B�{B���B��B��B�{B���B��B�B�z�B��B��B�=qB��\B�33B�B�Q�B��B��B�Q�B���B�33B�B�Q�B���B���B�(�B���B�G�B��B�Q�B��RB�\)B�{B��RB�G�B��B�Q�B���B�\)B��B���B�\)B��B�ffB��HB��B�  B��HB�p�B�  B�Q�B��HB��B�=qB���B�G�B��B�Q�B��HB��B�=qB���B��B��B�=qB���B��B�{B�z�B�
=B���B�ffB�
=B��B��B�z�B��B��
B���B�G�B�B�=qB���B�p�B�(�B��HB��B��B\B�
=B��Bď\B�33BŮB�(�BƸRBǙ�B�(�Bȣ�B�33B��
B�ffB�33B�B�{B̸RB�33B�  BΣ�B�G�BϮB�=qB���Bљ�B�=qB��HB�p�B��B�z�B�33B��B֏\B���BׅB�(�B���Bٙ�B�Q�BڸRB�G�B��B܏\B�p�B�(�Bޏ\B�G�B��B�RB�p�B��
B�z�B�33B�  B��B��B�B�z�B�G�B�  B�ffB�
=B�B�z�B�33B뙚B�=qB��HB�B�ffB��HB�p�B�=qB���B�B�=qB�RB�\)B�=qB���B�p�B�{B���B���B�  B��RB���B�(�B��RB�p�B�Q�B���B��B�Q�B�33B���C (�C ��C ��C33C�C  CQ�C�\C��C\)C��C��CffCC��C\)CC
=CQ�C��C�CffCC(�C\)C�C	{C	Q�C	�C	�
C
{C
(�C
G�C
��C
�RC
�RC
�HC  C
=C
=C(�C=qCQ�CG�Cp�C�Cp�C�C�C�C�RC�HC��C�C��C(�C=qC33C=qCp�Cz�Cz�C��C�RC�C�RC��C  C  C33CQ�CG�Cp�C�\C�C��C�
C��C�C(�C(�C=qCp�C�\C�C��C�
C�
C�C(�C=qC=qCp�C��C�\C��C�C�C�C=qC=qCp�C��C��C�RC�C�C�CG�C=qCz�C��C��C�
C��C��C(�CQ�CQ�Cz�C�C�C��C  C��C�CQ�CG�CffC��C��C�C�HC�HC
=C33C(�C\)Cp�Cp�C��C�C�C�C�C�C=qC=qCffCp�C�C�RCC��C
=C{C�CQ�C\)Cp�C��C�RC�RC�C��C  C33C(�C=qCp�CffC��C��C�C�
C�
C�HC�C�CG�CffC\)C��C��CC�C�C�C�C33CffCp�C��C�RCC�C�C
=C=qC=qCp�Cp�C�\CCC��C��C{CG�C=qCz�Cz�C��C�RC�
C  C  C33C33CffCz�C�CCC  C  C{C\)C\)C�\C��C��C�HC��C=qCG�Cz�C�CC�HC��C=qCG�C�C��C�
C��C {C \)C ffC �C C!
=C!�C!\)C!�C!��C!�C!��C"G�C"\)C"��C"�C#  C#{C#=qC#�C#��C#�C#��C$G�C$ffC$�C$C%{C%33C%ffC%�C%C&�C&(�C&�C&��C&��C'{C'ffC'�C'�
C'��C(G�C(ffC(�RC(�HC)33C)Q�C)��C)C*{C*33C*�\C*�C*��C+(�C+p�C+��C+�
C,{C,Q�C,�C,�RC-  C-{C-p�C-�C-�HC-�C.G�C.\)C.�C.��C/�C/G�C/��C/�RC0
=C033C0�C0��C1  C1(�C1ffC1��C1��C2(�C2=qC2��C2�RC3{C3(�C3�\C3�C4  C4�C4z�C4�\C4�C5
=C5\)C5�C5�
C5��C6G�C6p�C6C6�C733C7\)C7�RC7��C833C8G�C8��C8C9{C9=qC9�C9C9�HC:=qC:\)C:�C:C;{C;33C;�C;��C;��C<{C<ffC<p�C<��C<�C=33C=Q�C=�\C=��C=�C>=qC>Q�C>��C>C?
=C?�C?p�C?�\C?�
C?�C@G�C@Q�C@��C@CA
=CA�CAz�CA�\CA��CB  CB(�CBp�CB�CB�
CB�CC=qCC\)CC��CC�RCD{CD(�CDffCD��CDCE
=CE(�CEp�CE�\CE�
CE��CF=qCFQ�CF��CFCF��CG=qCG\)CG�CGG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411141114114114111411141114114111411411411411411141141114114114114114114114114114114114114114114114114114114141141111111111111141111411411114114114141141411111114114141141411414111141411114141141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�=qA�(�AԮA���AҶFA�VA�E�A�5?A�-A�$�A��A��A�bA�A���A���A��A��A��yA��`A��;A���A�ȴAѸRAѡ�A�|�A�^5A�E�A�/A�(�A�{A���A���A���A��`A�z�A�=qA�33A�VA�n�AΥ�A�z�A�"�Aͧ�A�+A�t�A�-A�A���A�r�A�~�A���AȓuA��A�AǙ�A�n�AƬA�hsA�Q�A�9XA���A�?}A��yA�S�A��FA��\A���A�&�A�ȴA�ffA��A�G�A���A�^5A��jA�;dA�  A�&�A�S�A���A��uA�`BA�%A�p�A�bA|�Azr�AtE�Anz�Ak��Ajn�Aip�Af��A`  A]��A[��AW�wAU�
AU�AT�AS�-AOAJbAG�AC|�AA��AA"�A?�A<��A;�A:~�A9��A8�HA7�A7��A7C�A5dZA0ĜA.ffA-�wA,�uA+�A(�RA'�;A'l�A&��A%�A$bNA"�A!"�A ��A�AQ�A  A33A{A�RAhsA"�A�HA�A/AE�A�;A�HA=qAG�A�A��A�DA��A9XA�AXA�A33AJA��Ax�A�A�uA1'A%A
n�A	�mA	��A	XA�AĜA�A"�AoA��Av�A��A��AC�A��A~�A�#A�A��A �yA 1@�o@��@�j@�J@�O�@��w@���@�~�@���@���@��@�=q@��@���@�!@��@��@�hs@�G�@�V@��@�!@�V@�$�@��@���@�@��@�/@��@��m@畁@�K�@��@��@�=q@��@�  @�|�@�+@�o@�!@���@�p�@�@���@�|�@�v�@݉7@�7L@ܛ�@�l�@�K�@��H@ى7@�/@ؼj@ؓu@ش9@��
@��H@�n�@�5?@���@��y@��@ڇ+@ۍP@�o@���@�l�@�"�@ڧ�@���@��@�A�@���@ם�@ָR@�$�@��@ӝ�@�
=@��@��@�@���@�@���@�n�@�`B@�G�@Ѻ^@�@���@ѡ�@�O�@Ь@��;@�|�@�+@���@Ώ\@�=q@ͺ^@��@�Ĝ@�z�@�bN@��m@�t�@�+@��@�o@�@ʧ�@�v�@��@���@�O�@�Z@Ǿw@Ə\@�$�@š�@�p�@�O�@�&�@ģ�@î@�33@��@�ȴ@�@�V@�hs@���@��j@��D@�j@�A�@��w@�\)@��@��!@�~�@�$�@���@�p�@�O�@��@���@��@� �@���@���@��@���@�r�@���@��/@��j@��D@�(�@�|�@��@��@��@��@��@��@��D@��F@��w@��@�
=@��T@�%@���@�Ĝ@��@�1'@�33@��R@�^5@�M�@�{@��@��-@�&�@�Z@���@�"�@�@��@��y@���@�@���@��7@�p�@��`@��D@�1'@��w@���@�(�@�dZ@�E�@��T@��@�%@��@�A�@�1@��;@���@���@�dZ@�"�@���@���@�E�@��@���@�p�@��/@���@���@�A�@��;@��
@��@��@�33@�^5@�p�@��/@�bN@��;@���@�|�@�S�@���@���@�V@�5?@�$�@��@�hs@�/@�%@�%@���@��@��@���@�;d@�o@��H@��!@�M�@�{@���@��7@�x�@�hs@�G�@�?}@�/@��/@��9@�A�@���@�o@���@�E�@�J@���@���@�O�@���@���@��u@�z�@�bN@�1@��F@���@���@�K�@���@�ȴ@��R@���@��+@�-@���@�p�@�&�@��@���@�A�@��@��@���@�t�@�C�@�"�@��@���@���@���@��+@�n�@�$�@��h@���@��@�Z@� �@�1@�1@���@��F@�dZ@�"�@�"�@�+@��@���@���@�V@�J@�@���@��h@�x�@��@��u@��@� �@��@�t�@�;d@���@���@��+@�v�@�@�O�@��9@�bN@�9X@�1@�P@\)@;d@
=@~�y@~ff@}��@}p�@|��@|�D@|z�@{�m@{C�@zn�@y��@yhs@yG�@y%@xr�@xQ�@w�@v�@v@u?}@t�/@tZ@t(�@s�
@sƨ@s��@sS�@s33@so@r��@rM�@r=q@q��@q�@p�`@pĜ@p�u@o\)@n�y@n�R@n$�@m��@mV@lZ@l9X@l�@k�m@k��@ko@j��@i�#@ix�@h�9@hr�@hbN@h �@hb@g�;@g|�@g+@f�y@f�R@fff@e/@cƨ@c@b�\@b^5@b=q@bJ@a�7@aX@a7L@`��@`b@`  @_��@_�@^v�@]�@]p�@\��@[�F@[@Z-@Y�7@X��@W��@W��@WK�@W
=@Vȴ@VE�@V{@V{@U�@U@U��@U�h@Up�@U�@T��@S�m@R�\@RM�@RJ@Q��@Q�@P�@O�@O�P@O;d@N��@Nv�@N5?@M�-@M�@M/@L�D@L9X@K��@K��@KdZ@K33@J�H@J��@Jn�@J=q@I�#@Ihs@H�@HA�@G�@GK�@F�y@F��@F�+@F5?@F5?@E�@E?}@D�j@Dj@D1@C��@C�F@C�@Co@B�!@B��@B~�@B^5@A��@AG�@@�`@@A�@?��@?\)@?;d@>��@>�y@>$�@=�@=O�@<�@<j@<�@;�m@;�F@;t�@;33@:��@:~�@:^5@:M�@:^5@:M�@:M�@:=q@9��@9x�@9hs@8��@8�9@8�@7�@7|�@7+@7�@7�@6�y@6�R@6ff@65?@5�@5p�@4�@4�@4�D@4(�@3��@3��@3dZ@2��@2n�@2M�@2M�@2J@1��@1x�@1G�@17L@1�@1%@0�`@0Ĝ@0�@0Q�@/�@/�P@/;d@/
=@.�@.ȴ@.��@.��@.��@.��@.�+@.5?@-�h@,��@,�@,�@+��@+��@+t�@+dZ@+C�@+@*��@*�\@*=q@*�@)�@)�^@)x�@)7L@(�9@(bN@'�@'��@'l�@'+@&ȴ@&�R@&�+@&V@%�@%�h@%O�@%/@$�@$��@$I�@#�F@#@"��@"~�@"�@"J@!��@!�^@!X@!&�@!�@!%@!%@ ��@ ��@ ��@ ��@ ��@ �9@ r�@ b@�;@��@;d@�@��@ȴ@ȴ@��@ff@{@�-@�h@�@?}@V@��@�@�/@�/@��@��@�D@I�@��@�@S�@33@@��@=q@�#@��@x�@x�@X@&�@�9@�@Q�@ �@b@  @�w@��@�P@l�@\)@
=@��@E�@@��@p�@O�@?}@?}@/@/@V@�D@z�@Z@�@��@��@�m@�
@�F@S�@C�@33@o@@�H@��@M�@-@J@��@�^@�7@7L@7L@7L@&�@&�@�@�`@��@��@Ĝ@��@�@r�@bN@A�@1'@1'@  @�;@��@�@�P@|�@K�@+@
=@�y@�R@��@��@�+@�+@ff@ff@E�@{@�T@�T@��@��@��@@��@�@`B@O�@?}@/@�@�@�/@�j@�@�D@Z@9X@(�@��@�m@�
@ƨ@�@dZ@S�@S�@C�@C�@S�@C�@33@
�@
��@
��@
�!@
^5@
J@	�@	��@	�^@	��@	hs@	hs@	X@	G�@	G�@	G�@	G�@	�@Ĝ@�u@r�@A�@ �@b@�;@��@��A�33A�33A�;dA�?}A�?}A�A�A�?}A�;dA�$�AԼjAԴ9A���Aԟ�Aԕ�A�r�A�33A�%A�ĜA�G�A�AҼjA҉7A�ffA�^5A�VA�M�A�K�A�I�A�E�A�?}A�=qA�9XA�9XA�7LA�33A�/A�/A�/A�/A�-A�&�A�&�A�"�A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A�bA�bA�VA�VA�JA�
=A�%A�%A�A�A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��mA��`A��mA��mA��mA��mA��mA��`A��HA��HA��;A��;A��;A��;A��;A��HA��HA��;A��/A��A���A���A���A���A���A���A���A���A���A���A���A�ȴA�A���AѾwAѺ^AѺ^AѸRAѶFAѴ9AѲ-AѰ!AѰ!AѰ!AѮAѧ�Aѡ�Aѝ�Aљ�AёhAыDAч+AуA�~�A�x�A�r�A�p�A�l�A�jA�ffA�^5A�\)A�XA�XA�VA�S�A�S�A�S�A�K�A�G�A�=qA�7LA�5?A�1'A�33A�1'A�1'A�1'A�-A�(�A�&�A�(�A�(�A�(�A�&�A�&�A�&�A�+A� �A��A��A��A�{A�oA�bA�JA�%A�  A���A���A���A���A���A���A���A���A���A���A���A�  A���A���A���A���A���A���A���A���A���A���A���A��A��A��;A���A�ȴA�Aв-AП�AЃA�t�A�O�A�E�A�C�A�A�A�?}A�=qA�=qA�;dA�7LA�7LA�;dA�7LA�7LA�5?A�33A�1'A�+A�&�A�&�A�$�A�$�A��A�A��A��A��`A���Aϰ!Aϝ�A�l�A�A�A��A���AθRAάAΧ�AΡ�AΝ�AΟ�AΛ�AΙ�AΓuA΍PA΅A�t�A�^5A�VA�I�A�9XA�(�A�"�A��A��A�
=A�  A��yA�A͝�A͏\A�x�A�hsA�\)A�K�A�=qA�7LA�/A��A���A���A̰!A�t�A�dZA�^5A�XA�K�A�E�A�;dA�5?A�5?A�(�A�&�A��A��A�bA�JA�1A�%A���A��A��A��HA��A���A���A˾wA˶FAˬA˥�Aˡ�A˗�A�n�A�?}A�oA��A��#A�ƨAʡ�A�^5A��A�A���A��A��A��mA�ĜAɩ�Aə�AɅA�p�A�{A�p�A� �A�%A�  A�  A���A���A��yA��/A��A��#A��
A���A���AǸRAǲ-Aǰ!AǬAǥ�Aǝ�AǗ�AǑhAǍPAǁA�z�A�t�A�r�A�p�A�n�A�hsA�VA��A���Aư!Aƣ�A�l�A�S�Aũ�A���A�|�A�A�A� �A��AîAÏ\A�hsA�XA�K�A�;dA��A��TA�ĜA�$�A��A��A��mA���A���A��9A���A��hA��DA��A�l�A�VA�A�A�(�A�$�A��A�A���A��A��A��A�ĜA���A�r�A�bNA�O�A�9XA��A���A���A��A��
A��^A�7LA��!A�G�A���A�E�A���A��hA�O�A�1A���A���A�ffA�-A��^A��A���A�1A���A�G�A�VA�5?A���A�`BA��
A��A�{A��A���A�hsA�1A��-A��
A�A���A�7LA���A���A�z�A�VA�=qA�1'A��A�  A���A��mA���A���A���A��hA��A�|�A�t�A�l�A�n�A�bNA�G�A�C�A�?}A�9XA�9XA�9XA�/A�$�A�"�A��A�bA�bA�A���A��A���A���A��A���A�ƨA���A�ȴA���A��jA�ƨA�ĜA��uA��7A�hsA�l�A��A�VA��A���A���A�ȴA��9A��A��A�p�A�-A�=qA�"�A�x�A�O�A�-A�ĜA��A�1A�&�A���A�5?A�"�A��A�
=A��`A���A�|�A�p�A�hsA�bNA�^5A�M�A�G�A�A��A��mA��/A���A��wA���A�VA�%A��A��A��yA��HA��jA���A�z�A�^5A�9XA�bA�A��A��A��;A�ĜA��!A�~�A�C�A�
=A��/A�ƨA���A���A��hA��DA��7A�|�A�ffA�O�A�A�A�?}A�9XA�-A��A�1A���A��A���A��-A��A�S�A�$�A�  A��A���A�^5A�t�A�jA���A��7A�5?A��mA���A���A��A���A�5?A�9XA�-A�$�A��A�oA�oA�1A�  A���A���A��`A��/A�ƨA��FA��!A��-A���A���A��DA�t�A�1'A��A��A�1'A��7A�;dA��A�
=A���A��yA���A�ƨA��RA���A�v�A�M�A�$�A�  A�ƨA��uA�M�A�A�A�1'A�-A��A��A���A���A��A�Q�A�{A��jA��A�r�A��A���A���A���A�jA�Q�A�C�A�1'A�"�A�VA��A���A�ffA���A�ȴA��A�~�A�33A�&�A��A�x�A���A� �A�A���A�hsA�oA��A���A��FA���A�v�A�`BA�Q�A�"�A�VAA~��A}�A}/A|��A|-A{�#A{��A{�-A{��A{t�A{S�A{33A{&�A{�Az�RAzz�Az�Ay�;Ay��Ayp�Ay&�AxAvv�AvAt�jAs��Ar�AqoAp��ApM�Ao��Ao|�Ao�An�An�jAn �Am�Al�AljAk�Ak�wAk�Ak��Ak��Ak��Ak�hAk�Ak`BAk"�Aj�Aj�DAjM�Aj=qAj5?Aj�Aj1Ai��Ai��AiƨAi�-Aip�AidZAiS�AiG�Ai�Ai%Ah�Ah��Ah��Ahv�Ah�Ag�Ae�
Act�Aa��AaK�A`ȴA`^5A`$�A_�TA_�FA_t�A^��A^�9A^�DA^ �A]�mA]�^A]x�A\��A\�jA\v�A\ZA\9XA\bA[�#A[�;A[�A[`BAZ�RAZ�AZI�AY�^AX�!AW7LAV��AV�uAVbNAV{AU��AU�AU�;AU�#AU��AU��AU��AUƨAU��AUAU�wAU��AU�AUp�AUt�AUl�AUl�AUS�AU;dAU"�AUoAUVAUVAU%AT�AT�/AT�RAT��AT�uATz�ATM�AT1'AS�AS��ASt�ASC�AS
=AR�9AR9XAQ��AQ?}APffAP{AO�;AO��AN��AN�DAM�-AL��AKƨAK33AJ�\AJE�AI�
AI|�AI?}AI�AH�AH��AH�jAH��AH~�AHQ�AH(�AG�AG�-AGO�AFĜAFA�AE|�AD�RAC��AC��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                          A�=qA�(�AԮA���AҶFA�VA�E�A�5?A�-A�$�A��A��A�bA�A���A���A��A��A��yA��`A��;A���A�ȴAѸRAѡ�A�|�A�^5A�E�A�/A�(�A�{A���A���A���A��`A�z�A�=qA�33A�VA�n�AΥ�A�z�A�"�Aͧ�A�+A�t�A�-A�A���A�r�A�~�A���AȓuA��A�AǙ�A�n�AƬA�hsA�Q�A�9XA���A�?}A��yA�S�A��FA��\A���A�&�A�ȴA�ffA��A�G�A���A�^5A��jA�;dA�  A�&�A�S�A���A��uA�`BA�%A�p�A�bA|�Azr�AtE�Anz�Ak��Ajn�Aip�Af��A`  A]��A[��AW�wAU�
AU�AT�AS�-AOAJbAG�AC|�AA��AA"�A?�A<��A;�A:~�A9��A8�HA7�A7��A7C�A5dZA0ĜA.ffA-�wA,�uA+�A(�RA'�;A'l�A&��A%�A$bNA"�A!"�A ��A�AQ�A  A33A{A�RAhsA"�A�HA�A/AE�A�;A�HA=qAG�A�A��A�DA��A9XA�AXA�A33AJA��Ax�A�A�uA1'A%A
n�A	�mA	��A	XA�AĜA�A"�AoA��Av�A��A��AC�A��A~�A�#A�A��A �yA 1@�o@��@�j@�J@�O�@��w@���@�~�@���@���@��@�=q@��@���@�!@��@��@�hs@�G�@�V@��@�!@�V@�$�@��@���@�@��@�/@��@��m@畁@�K�@��@��@�=q@��@�  @�|�@�+@�o@�!@���@�p�@�@���@�|�@�v�@݉7@�7L@ܛ�@�l�@�K�@��H@ى7@�/@ؼj@ؓu@ش9@��
@��H@�n�@�5?@���@��y@��@ڇ+@ۍP@�o@���@�l�@�"�@ڧ�@���@��@�A�@���@ם�@ָR@�$�@��@ӝ�@�
=@��@��@�@���@�@���@�n�@�`B@�G�@Ѻ^@�@���@ѡ�@�O�@Ь@��;@�|�@�+@���@Ώ\@�=q@ͺ^@��@�Ĝ@�z�@�bN@��m@�t�@�+@��@�o@�@ʧ�@�v�@��@���@�O�@�Z@Ǿw@Ə\@�$�@š�@�p�@�O�@�&�@ģ�@î@�33@��@�ȴ@�@�V@�hs@���@��j@��D@�j@�A�@��w@�\)@��@��!@�~�@�$�@���@�p�@�O�@��@���@��@� �@���@���@��@���@�r�@���@��/@��j@��D@�(�@�|�@��@��@��@��@��@��@��D@��F@��w@��@�
=@��T@�%@���@�Ĝ@��@�1'@�33@��R@�^5@�M�@�{@��@��-@�&�@�Z@���@�"�@�@��@��y@���@�@���@��7@�p�@��`@��D@�1'@��w@���@�(�@�dZ@�E�@��T@��@�%@��@�A�@�1@��;@���@���@�dZ@�"�@���@���@�E�@��@���@�p�@��/@���@���@�A�@��;@��
@��@��@�33@�^5@�p�@��/@�bN@��;@���@�|�@�S�@���@���@�V@�5?@�$�@��@�hs@�/@�%@�%@���@��@��@���@�;d@�o@��H@��!@�M�@�{@���@��7@�x�@�hs@�G�@�?}@�/@��/@��9@�A�@���@�o@���@�E�@�J@���@���@�O�@���@���@��u@�z�@�bN@�1@��F@���@���@�K�@���@�ȴ@��R@���@��+@�-@���@�p�@�&�@��@���@�A�@��@��@���@�t�@�C�@�"�@��@���@���@���@��+@�n�@�$�@��h@���@��@�Z@� �@�1@�1@���@��F@�dZ@�"�@�"�@�+@��@���@���@�V@�J@�@���@��h@�x�@��@��u@��@� �@��@�t�@�;d@���@���@��+@�v�@�@�O�@��9@�bN@�9X@�1@�P@\)@;d@
=@~�y@~ff@}��@}p�@|��@|�D@|z�@{�m@{C�@zn�@y��@yhs@yG�@y%@xr�@xQ�@w�@v�@v@u?}@t�/@tZ@t(�@s�
@sƨ@s��@sS�@s33@so@r��@rM�@r=q@q��@q�@p�`@pĜ@p�u@o\)@n�y@n�R@n$�@m��@mV@lZ@l9X@l�@k�m@k��@ko@j��@i�#@ix�@h�9@hr�@hbN@h �@hb@g�;@g|�@g+@f�y@f�R@fff@e/@cƨ@c@b�\@b^5@b=q@bJ@a�7@aX@a7L@`��@`b@`  @_��@_�@^v�@]�@]p�@\��@[�F@[@Z-@Y�7@X��@W��@W��@WK�@W
=@Vȴ@VE�@V{@V{@U�@U@U��@U�h@Up�@U�@T��@S�m@R�\@RM�@RJ@Q��@Q�@P�@O�@O�P@O;d@N��@Nv�@N5?@M�-@M�@M/@L�D@L9X@K��@K��@KdZ@K33@J�H@J��@Jn�@J=q@I�#@Ihs@H�@HA�@G�@GK�@F�y@F��@F�+@F5?@F5?@E�@E?}@D�j@Dj@D1@C��@C�F@C�@Co@B�!@B��@B~�@B^5@A��@AG�@@�`@@A�@?��@?\)@?;d@>��@>�y@>$�@=�@=O�@<�@<j@<�@;�m@;�F@;t�@;33@:��@:~�@:^5@:M�@:^5@:M�@:M�@:=q@9��@9x�@9hs@8��@8�9@8�@7�@7|�@7+@7�@7�@6�y@6�R@6ff@65?@5�@5p�@4�@4�@4�D@4(�@3��@3��@3dZ@2��@2n�@2M�@2M�@2J@1��@1x�@1G�@17L@1�@1%@0�`@0Ĝ@0�@0Q�@/�@/�P@/;d@/
=@.�@.ȴ@.��@.��@.��@.��@.�+@.5?@-�h@,��@,�@,�@+��@+��@+t�@+dZ@+C�@+@*��@*�\@*=q@*�@)�@)�^@)x�@)7L@(�9@(bN@'�@'��@'l�@'+@&ȴ@&�R@&�+@&V@%�@%�h@%O�@%/@$�@$��@$I�@#�F@#@"��@"~�@"�@"J@!��@!�^@!X@!&�@!�@!%@!%@ ��@ ��@ ��@ ��@ ��@ �9@ r�@ b@�;@��@;d@�@��@ȴ@ȴ@��@ff@{@�-@�h@�@?}@V@��@�@�/@�/@��@��@�D@I�@��@�@S�@33@@��@=q@�#@��@x�@x�@X@&�@�9@�@Q�@ �@b@  @�w@��@�P@l�@\)@
=@��@E�@@��@p�@O�@?}@?}@/@/@V@�D@z�@Z@�@��@��@�m@�
@�F@S�@C�@33@o@@�H@��@M�@-@J@��@�^@�7@7L@7L@7L@&�@&�@�@�`@��@��@Ĝ@��@�@r�@bN@A�@1'@1'@  @�;@��@�@�P@|�@K�@+@
=@�y@�R@��@��@�+@�+@ff@ff@E�@{@�T@�T@��@��@��@@��@�@`B@O�@?}@/@�@�@�/@�j@�@�D@Z@9X@(�@��@�m@�
@ƨ@�@dZ@S�@S�@C�@C�@S�@C�@33@
�@
��@
��@
�!@
^5@
J@	�@	��@	�^@	��@	hs@	hs@	X@	G�@	G�@	G�@	G�@	�@Ĝ@�u@r�@A�@ �@b@�;@��@��A�33A�33A�;dA�?}A�?}A�A�A�?}A�;dA�$�AԼjAԴ9A���Aԟ�Aԕ�A�r�A�33A�%A�ĜA�G�A�AҼjA҉7A�ffA�^5A�VA�M�A�K�A�I�A�E�A�?}A�=qA�9XA�9XA�7LA�33A�/A�/A�/A�/A�-A�&�A�&�A�"�A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A�bA�bA�VA�VA�JA�
=A�%A�%A�A�A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��mA��`A��mA��mA��mA��mA��mA��`A��HA��HA��;A��;A��;A��;A��;A��HA��HA��;A��/A��A���A���A���A���A���A���A���A���A���A���A���A�ȴA�A���AѾwAѺ^AѺ^AѸRAѶFAѴ9AѲ-AѰ!AѰ!AѰ!AѮAѧ�Aѡ�Aѝ�Aљ�AёhAыDAч+AуA�~�A�x�A�r�A�p�A�l�A�jA�ffA�^5A�\)A�XA�XA�VA�S�A�S�A�S�A�K�A�G�A�=qA�7LA�5?A�1'A�33A�1'A�1'A�1'A�-A�(�A�&�A�(�A�(�A�(�A�&�A�&�A�&�A�+A� �A��A��A��A�{A�oA�bA�JA�%A�  A���A���A���A���A���A���A���A���A���A���A���A�  A���A���A���A���A���A���A���A���A���A���A���A��A��A��;A���A�ȴA�Aв-AП�AЃA�t�A�O�A�E�A�C�A�A�A�?}A�=qA�=qA�;dA�7LA�7LA�;dA�7LA�7LA�5?A�33A�1'A�+A�&�A�&�A�$�A�$�A��A�A��A��A��`A���Aϰ!Aϝ�A�l�A�A�A��A���AθRAάAΧ�AΡ�AΝ�AΟ�AΛ�AΙ�AΓuA΍PA΅A�t�A�^5A�VA�I�A�9XA�(�A�"�A��A��A�
=A�  A��yA�A͝�A͏\A�x�A�hsA�\)A�K�A�=qA�7LA�/A��A���A���A̰!A�t�A�dZA�^5A�XA�K�A�E�A�;dA�5?A�5?A�(�A�&�A��A��A�bA�JA�1A�%A���A��A��A��HA��A���A���A˾wA˶FAˬA˥�Aˡ�A˗�A�n�A�?}A�oA��A��#A�ƨAʡ�A�^5A��A�A���A��A��A��mA�ĜAɩ�Aə�AɅA�p�A�{A�p�A� �A�%A�  A�  A���A���A��yA��/A��A��#A��
A���A���AǸRAǲ-Aǰ!AǬAǥ�Aǝ�AǗ�AǑhAǍPAǁA�z�A�t�A�r�A�p�A�n�A�hsA�VA��A���Aư!Aƣ�A�l�A�S�Aũ�A���A�|�A�A�A� �A��AîAÏ\A�hsA�XA�K�A�;dA��A��TA�ĜA�$�A��A��A��mA���A���A��9A���A��hA��DA��A�l�A�VA�A�A�(�A�$�A��A�A���A��A��A��A�ĜA���A�r�A�bNA�O�A�9XA��A���A���A��A��
A��^A�7LA��!A�G�A���A�E�A���A��hA�O�A�1A���A���A�ffA�-A��^A��A���A�1A���A�G�A�VA�5?A���A�`BA��
A��A�{A��A���A�hsA�1A��-A��
A�A���A�7LA���A���A�z�A�VA�=qA�1'A��A�  A���A��mA���A���A���A��hA��A�|�A�t�A�l�A�n�A�bNA�G�A�C�A�?}A�9XA�9XA�9XA�/A�$�A�"�A��A�bA�bA�A���A��A���A���A��A���A�ƨA���A�ȴA���A��jA�ƨA�ĜA��uA��7A�hsA�l�A��A�VA��A���A���A�ȴA��9A��A��A�p�A�-A�=qA�"�A�x�A�O�A�-A�ĜA��A�1A�&�A���A�5?A�"�A��A�
=A��`A���A�|�A�p�A�hsA�bNA�^5A�M�A�G�A�A��A��mA��/A���A��wA���A�VA�%A��A��A��yA��HA��jA���A�z�A�^5A�9XA�bA�A��A��A��;A�ĜA��!A�~�A�C�A�
=A��/A�ƨA���A���A��hA��DA��7A�|�A�ffA�O�A�A�A�?}A�9XA�-A��A�1A���A��A���A��-A��A�S�A�$�A�  A��A���A�^5A�t�A�jA���A��7A�5?A��mA���A���A��A���A�5?A�9XA�-A�$�A��A�oA�oA�1A�  A���A���A��`A��/A�ƨA��FA��!A��-A���A���A��DA�t�A�1'A��A��A�1'A��7A�;dA��A�
=A���A��yA���A�ƨA��RA���A�v�A�M�A�$�A�  A�ƨA��uA�M�A�A�A�1'A�-A��A��A���A���A��A�Q�A�{A��jA��A�r�A��A���A���A���A�jA�Q�A�C�A�1'A�"�A�VA��A���A�ffA���A�ȴA��A�~�A�33A�&�A��A�x�A���A� �A�A���A�hsA�oA��A���A��FA���A�v�A�`BA�Q�A�"�A�VAA~��A}�A}/A|��A|-A{�#A{��A{�-A{��A{t�A{S�A{33A{&�A{�Az�RAzz�Az�Ay�;Ay��Ayp�Ay&�AxAvv�AvAt�jAs��Ar�AqoAp��ApM�Ao��Ao|�Ao�An�An�jAn �Am�Al�AljAk�Ak�wAk�Ak��Ak��Ak��Ak�hAk�Ak`BAk"�Aj�Aj�DAjM�Aj=qAj5?Aj�Aj1Ai��Ai��AiƨAi�-Aip�AidZAiS�AiG�Ai�Ai%Ah�Ah��Ah��Ahv�Ah�Ag�Ae�
Act�Aa��AaK�A`ȴA`^5A`$�A_�TA_�FA_t�A^��A^�9A^�DA^ �A]�mA]�^A]x�A\��A\�jA\v�A\ZA\9XA\bA[�#A[�;A[�A[`BAZ�RAZ�AZI�AY�^AX�!AW7LAV��AV�uAVbNAV{AU��AU�AU�;AU�#AU��AU��AU��AUƨAU��AUAU�wAU��AU�AUp�AUt�AUl�AUl�AUS�AU;dAU"�AUoAUVAUVAU%AT�AT�/AT�RAT��AT�uATz�ATM�AT1'AS�AS��ASt�ASC�AS
=AR�9AR9XAQ��AQ?}APffAP{AO�;AO��AN��AN�DAM�-AL��AKƨAK33AJ�\AJE�AI�
AI|�AI?}AI�AH�AH��AH�jAH��AH~�AHQ�AH(�AG�AG�-AGO�AFĜAFA�AE|�AD�RAC��AC��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
UgB
W�B
S�B
X�B
XB
QNB
QB
QNB
P�B
QB
QB
QB
QNB
Q�B
Q�B
Q�B
Q�B
R B
R B
RTB
R�B
S�B
T�B
VB
W
B
ZQB
]�B
^�B
\�B
_B
a|B
_�B
d�B
h�B
jB
iDB
lWB
qB
�:B
��BqBE�B_;BtTB�uB�nB��B�)BϫB�QB��B�.BB�B \B!bB!-B,BOBBM�Bh�B�B�B|�Bu%Bj�BF?B.B�Bm�B�B
��B
�KB
B
�XB
��B
�rB
poB
qvB
dZB
A�B
�B
�B	�%B	��B	�GB	�;B	�XB	�RB	��B	�;B	{�B	sMB	o�B	S�B	LdB	E�B	B�B	7B	5�B	2�B	-�B	"�B		�B��B��B�sB�B��B�B�TB�B��B�B�/B��B�EB��B�#B՛BԕB��B��B�EB�2B�gB�yB�EB��B��B�[BѷB��B�pB�jB�#B�HB�
B�yB�B��B�B�TB�>B	�B	oB�VB�xB�JB�DB	JB	<�B	v`B	��B	��B	��B	��B	��B	��B	��B	��B	�:B	�nB	��B	��B	�tB	�B	��B	��B	�B	�-B	�'B	��B	�hB	�FB	�LB	�tB	�B	�RB	�B	��B	��B	�B	�B	�[B	��B	��B	��B	�kB	�XB	�eB	�$B	�XB	��B	�eB	��B	�B	��B	��B	�B	��B	��B	��B	��B	�!B	�B	��B	��B	�*B	��B	��B	�B	�6B	�B	�wB	��B	��B	��B	��B	��B	�'B	��B	��B	�gB	��B	ÖB	�3B	�gB	�gB	�?B	�B	�tB	ɆB	��B	�0B	�HB	�jB	�6B	��B	��B	�B	�B	�<B	�vB	ҽB	�pB	�B	ΥB	��B	�B	�B	��B
�B
MB
B
+B
�B
�B
B
�B	��B	�(B	��B	�(B	��B	��B	��B	�B	��B	�rB	�B	�JB	�B	��B	��B	�]B	�(B
 �B
�B
�B
{B
�B
�B
�B
�B
MB
B
MB
B
�B
�B
�B
�B
GB
�B
�B
�B
�B
�B
MB
�B
�B
MB
B
�B
B
MB
�B
GB
AB
B
�B
�B
�B
B
�B
�B
B
MB
�B
+B
�B
�B
�B
YB
�B
�B
_B
_B
+B
�B
�B
_B
�B
�B
�B
�B
_B
%B
_B
�B
�B
�B
GB
�B
+B
	B
�B
�B
+B
�B
B
�B	��B
B
AB
�B	��B
  B
 �B	�cB	�VB	��B	��B	�"B	��B	�VB	��B	��B	��B	�	B	�B	�8B	�fB	�8B	��B	�JB	��B	�JB	��B	�B	��B	��B	��B	��B	�"B	�cB	��B
oB
uB
B
�B
�B
�B
MB
YB
	B

=B
	7B
�B
�B
fB
fB
�B
1B
�B
1B
�B
	B
�B
	�B

�B

�B

�B
�B
~B
JB
�B
~B
�B
"B
(B
�B
4B
:B
�B
�B
�B
uB
�B
�B
�B
{B
B
�B
$B
�B
B
�B
�B
YB
�B
1B
1B
�B
1B
B
7B
B
B
�B
	B
qB
=B
=B
�B
�B
�B
B
�B
B
�B
�B
!B
�B
�B
 \B
 'B
 �B
 �B
 'B
!bB
!�B
"4B
!bB
#B
"hB
"hB
#:B
"hB
"4B
"�B
#�B
#�B
#�B
#�B
$tB
%zB
%zB
%�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
'B
'�B
(�B
*0B
)_B
)�B
*eB
*�B
*0B
*0B
+6B
+kB
,qB
,�B
-�B
.�B
.�B
/B
/�B
/�B
1[B
1'B
1[B
1�B
2�B
2-B
2aB
3�B
2�B
2�B
33B
3�B
4�B
4�B
4nB
5tB
6B
6B
5�B
5?B
5?B
5�B
6B
6B
6B
6FB
7�B
7�B
7LB
8�B
9$B
9XB
:�B
:*B
:�B
:�B
:�B
:*B
;0B
;0B
:�B
;0B
<B
<�B
<6B
<6B
=<B
<�B
<�B
=B
=B
=qB
=B
=B
>BB
>B
=qB
=�B
>wB
>B
>B
>BB
?�B
?}B
@B
@B
@B
@OB
@�B
@OB
@OB
@OB
@�B
@�B
AUB
A�B
B'B
C-B
B�B
B�B
C-B
C-B
CaB
C�B
D3B
C�B
CaB
C-B
B�B
B'B
B'B
B�B
C-B
C�B
C�B
DgB
DgB
DgB
E�B
EB
E�B
GB
H�B
I�B
I�B
J#B
K�B
K^B
JXB
I�B
IB
HKB
H�B
H�B
I�B
J#B
M6B
P}B
P}B
PHB
QB
QB
P�B
P}B
PHB
QB
QB
R�B
S[B
R�B
RTB
S�B
S�B
TaB
TaB
T�B
T�B
T�B
TaB
TaB
TaB
T�B
V�B
YB
YKB
YKB
YKB
Y�B
Y�B
Z�B
Y�B
ZB
Y�B
Z�B
Z�B
[#B
[WB
[�B
\�B
\�B
\�B
]/B
]�B
\�B
]�B
^B
^�B
^jB
_;B
^jB
_pB
_;B
_�B
`B
`B
_�B
_�B
`�B
`�B
aB
a�B
bNB
bB
b�B
bNB
a�B
c B
cTB
cTB
c�B
d&B
d�B
d&B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e`B
e�B
e�B
e�B
ffB
ffB
e�B
gB
f�B
f�B
g�B
g�B
h
B
h
B
g�B
h
B
h>B
h
B
hsB
hsB
h�B
iyB
iDB
iDB
i�B
jKB
jKB
jKB
kQB
k�B
k�B
kQB
k�B
l"B
lWB
lWB
lWB
l�B
l"B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n/B
n/B
ncB
n/B
n/B
n/B
m�B
n/B
o B
oiB
o B
pB
pB
o�B
pB
o�B
pB
poB
pB
p�B
qB
p�B
qB
qB
qB
qvB
rGB
rB
rB
r|B
r�B
sB
sMB
sB
sMB
s�B
s�B
tTB
tTB
tB
t�B
t�B
t�B
u�B
v+B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
wfB
w�B
w�B
w�B
w�B
w�B
w�B
wfB
w�B
w�B
xB
xB
xlB
y	B
yrB
x�B
y	B
y�B
y�B
yrB
y�B
zB
z�B
zxB
z�B
z�B
z�B
{B
{B
z�B
{B
{B
{JB
{B
{B
{B
|PB
|PB
|B
|�B
|�B
|�B
}VB
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
cB
�B
� B
�4B
��B
��B
��B
��B
��B
�B
��B
�B
��B
��B
��B
�AB
�AB
�AB
�AB
�uB
�uB
�B
��B
�B
�B
�GB
�{B
�{B
�MB
�MB
��B
�MB
�B
��B
�%B
��B
�%B
�%B
�%B
�%B
��B
��B
�YB
��B
��B
��B
��B
�+B
�+B
�+B
��B
�_B
��B
��B
��B
��B
��B
�1B
�fB
�fB
��B
��B
�B
�B
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
��B
��B
�	B
�=B
�rB
�=B
�rB
�rB
��B
�B
�B
�B
�DB
�DB
�xB
�xB
�xB
��B
�xB
��B
��B
�B
�B
�JB
�B
�JB
�JB
�JB
�B
�B
�JB
�JB
�B
�B
��B
�PB
�B
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
�\B
��B
��B
��B
�.B
��B
�bB
�4B
�4B
� B
S�B
S�B
W�B
VmB
V�B
T�B
V9B
U2B
T�B
_B
U2B
QNB
XyB
S&B
R�B
R�B
T�B
W
B
S�B
OvB
gmB
W
B
S�B
QNB
S&B
Q�B
Q�B
Q�B
RTB
Q�B
Q�B
Q�B
QB
QB
Q�B
P�B
O�B
O�B
O�B
O�B
QNB
P�B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
P�B
QB
P�B
P}B
P�B
PHB
PHB
PHB
P}B
P�B
P}B
QB
P�B
QNB
QNB
R B
R B
R�B
RTB
RTB
R B
Q�B
QB
QB
P�B
PHB
P�B
QB
R B
Q�B
R�B
R�B
RTB
Q�B
Q�B
QNB
QNB
QNB
P}B
P�B
P�B
QB
P�B
P�B
Q�B
RTB
R�B
R�B
R�B
R�B
RTB
R�B
Q�B
Q�B
QNB
QNB
Q�B
RTB
R�B
R�B
S&B
Q�B
Q�B
Q�B
QNB
Q�B
R B
S&B
S[B
S�B
S�B
S&B
R�B
R�B
RTB
R B
R�B
S[B
S�B
T�B
T�B
T,B
S[B
S�B
S�B
S�B
T,B
S[B
S�B
S�B
T�B
U�B
U�B
V9B
V�B
V�B
V�B
WsB
W?B
WsB
V�B
VmB
U�B
U�B
VmB
V�B
V�B
W�B
X�B
YKB
YB
Y�B
ZQB
ZQB
Z�B
[�B
[WB
[�B
\�B
^jB
^jB
_;B
_B
^�B
^�B
^5B
]�B
`BB
^B
_pB
^B
^5B
]�B
]�B
]dB
\�B
\�B
[�B
\]B
[WB
[�B
^B
^�B
_�B
`B
`BB
bB
bNB
`�B
aB
`vB
`�B
`B
`�B
`�B
`�B
`B
_�B
_;B
_pB
^5B
_�B
`�B
a|B
b�B
e,B
d�B
f2B
g8B
g�B
hsB
h�B
h�B
h�B
h�B
h>B
h
B
h�B
iDB
jB
kB
kB
l�B
k�B
h�B
hsB
iB
i�B
f�B
iDB
jKB
g�B
iyB
jB
k�B
l�B
l�B
m�B
n/B
ncB
m�B
n/B
m)B
o B
q�B
t�B
w�B
{�B
�B
��B
��B
��B
�kB
��B
��B
��B
��B
��B
��B
��B
��B
�8B
�PBB+BB�B �B,qB;�B<jB>B@BB[BH�BM�BP�BS�BX�B^5B_�Ba|Bc�Bd�Bg�Bk�Bt�By>Bx�BxBx�By�B}VB}�B�B��B�%B�B��B�B�B�^B��B�qB��B��B�9BƨB�B�EB�EB��B�KB��BɺB˒B�)B�^B��B�^B�B�BуB�vB�NB�B��BӏB�,B�gB��B�BB�`B�B�"B��B�AB��B��B��B�VB��B�B��BuBuB �BoB�BB*0B \BOB~B�BIBB�B �B�B�BOB \B�B!�B!-B!-B \B 'B!-B!�B!�B!bB!�B!bB!�B"hB �B�B!B!�B)*B.}B)*B($B1�B+6BYBS[BGzBF�BI�BK)BM�BLdBNpBK�BK�BL�BQNBS�BR�Bp�Bj�BpBy�B��B��B�DB�B��B��B�rB�1B��B�MB��B�B�iB|�B�iB~]B}"B|Bw2By�B|�Bv�Bt�Bq�BpBqABiyBg�BgBd�Bl�B��BD3B?HB8�B/�B)*B#:B�B�B�B�B��B�B�B�B��B��B��B��B~�BYBrBU�BZ�B7B{B
=B�B1B#:BJB
یB
�9B
ߤB
�pB
�B
ΥB
ǮB
�B
�B
ĜB
�tB
�UB
��B
��B
�B
�gB
��B
�<B
��B
�B
�0B
�$B
�B
��B
��B
��B
�9B
��B
��B
�B
�9B
��B
�tB
�-B
�[B
�?B
��B
�UB
��B
��B
�wB
��B
�}B
�B
��B
��B
�B
�$B
�qB
��B
�0B
�eB
��B
��B
��B
�IB
�:B
��B
��B
�B
��B
��B
��B
�1B
�rB
�JB
��B
�B
��B
�B
uZB
��B
�uB
�=B
{�B
r�B
q�B
rB
s�B
v�B
qB
m�B
l�B
m�B
k�B
ncB
n/B
{JB
m]B
n�B
m�B
o�B
o�B
q�B
x�B
t�B
m�B
oiB
qB
r�B
xlB
r�B
s�B
tTB
t�B
r�B
p�B
o B
m�B
qvB
o�B
l"B
v`B
pB
l�B
l�B
kB
ncB
g�B
f�B
e,B
d&B
e`B
g8B
b�B
b�B
_�B
aB
b�B
a�B
bB
`vB
^5B
aB
^jB
a�B
_B
Y�B
W�B
T,B
T,B
[#B
e�B
VmB
O�B
5?B
49B
1�B
*0B
5tB
5tB
$@B
�B
_B
�B
�B
�B
_B
+B
+B
�B
�B
�B
_B
�B
FB
uB
�B
4B
�B
bB
B
PB
�B
�B
�B
�B
�B
(B
�B
fB
+B
YB
1B
B
�B
+B
MB
B
�B
B
�B
 �B	�B	�fB	�8B	��B	��B	�B	�+B	�+B	�B	�B	�B	��B
oB	�JB	��B	�B	�TB	�B	�B	�iB	�iB	�B	�B	��B	�WB	�oB	�xB	�B	�]B	�B	��B	�+B	�B	��B
�B
GB
B	��B	��B	�`B	��B	��B	�rB	��B	��B	�]B	�B	��B	�sB	��B	�DB	�]B	�;B	� B	�&B	�B	��B	�aB	ԕB	�B	��B	�}B	͟B	�jB	͟B	��B	�^B	��B	�mB	�3B	�B	�-B	֡B	רB	��B	ȴB	�B	��B	��B	��B	��B	�xB	�	B	�B	��B	��B	�qB	��B	��B	�PB	��B	��B	��B	cB	�iB	cB	�4B	}�B	}�B	�oB	��B	�iB	{�B	yrB	xB	x�B	x�B	w2B	tTB	u�B	u�B	v�B	q�B	rB	qAB	r�B	m�B	m�B	m�B	jB	iB	i�B	l"B	�B	�B	`B	Z�B	X�B	U2B	PB	R�B	M�B	R�B	S�B	OvB	JXB	P�B	H�B	I�B	M6B	P�B	I�B	IB	E�B	EmB	H�B	C�B	A B	C-B	B�B	JXB	@�B	C�B	L0B	R B	RTB	=B	6�B	<�B	9�B	8�B	6FB	9�B	7�B	8RB	6�B	6FB	6zB	5�B	6zB	6B	9$B	6B	6zB	3�B	5B	4B	6�B	4�B	5B	33B	2�B	1�B	2aB	49B	2-B	2�B	0!B	0!B	0!B	0UB	-wB	2-B	-CB	,qB	,=B	&B	0!B	)�B	%FB	/OB	*eB	�B	xB	CB	 �B	YB	#�B	$�B	eB	�B	�B	1B	�B	�B	�B	 iB��B�cB��B��B�B�VB��B��B�DB��B��B��B��B��B�	B�cG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                          B
MsB
O�B
K�B
P�B
PB
IZB
I&B
IZB
H�B
I&B
I&B
I&B
IZB
I�B
I�B
I�B
I�B
J,B
J,B
J`B
J�B
K�B
L�B
NB
OB
R]B
U�B
V�B
T�B
WB
Y�B
W�B
]B
`�B
b"B
aPB
dcB
iB
�FB
��B}B=�BWGBl`Bz�B�zB��B�5BǷB�]B��B�:BB�BhBnB9B$BGNBE�B`�B�B}+Bt�Bm1Bb�B>KB:B�Be�B
�B
��B
�WB
��B
�dB
��B
�~B
h{B
i�B
\fB
9�B
�B	��B	�1B	��B	�SB	�GB	�dB	�^B	��B	yGB	s�B	kYB	g�B	K�B	DpB	=�B	;B	/#B	-�B	*�B	%�B	�B	�B�B��B�B�B��BާB�`B�B��B�B�;B��B�QB��B�/BͧB̡B��B��B�QB�>B�sBЅB�QB��B��B�gB��B��B�|B�vB�/B�TB�B�B�B��B�B�`B�JB��B�{B�bB�B�VB�PB	VB	4�B	nlB	��B	��B	��B	��B	��B	��B	��B	��B	�FB	�zB	��B	��B	��B	�B	��B	��B	�'B	�9B	�3B	��B	�tB	�RB	�XB	��B	�*B	�^B	�*B	�B	��B	�B	�'B	�gB	��B	��B	��B	�wB	�dB	�qB	�0B	�dB	��B	�qB	��B	�B	��B	��B	�'B	��B	��B	��B	��B	�-B	�#B	��B	��B	�6B	��B	��B	�B	�BB	�B	��B	��B	��B	��B	��B	��B	�3B	��B	��B	�sB	��B	��B	�?B	�sB	�sB	�KB	�B	��B	��B	��B	�<B	�TB	�vB	�BB	��B	��B	�B	�B	�HB	ǂB	��B	�|B	�B	ƱB	��B	�B	�B	��B	��B	�YB	�B	�7B
 	B	��B	�+B	��B	�B	�4B	� B	�4B	��B	�B	��B	�B	�B	�~B	�"B	�VB	�B	��B	��B	�iB	�4B	��B	��B	��B	��B	��B	��B	��B	��B	�YB	�%B	�YB	�%B	��B	��B	��B	��B	�SB	��B	��B	��B	��B	��B	�YB	��B	��B	�YB	�B	��B	�%B	�YB	��B	�SB	�MB	�B	��B	��B	��B	�%B	��B	��B	�%B	�YB	��B	�7B	��B	��B	��B	�eB	��B	��B	�kB	�kB	�7B	�B	�B	�kB	��B	��B	��B
 	B	�kB	�1B	�kB
 	B	��B	��B	�SB	��B	�7B
B
 �B
 �B	�7B	��B	�%B	��B	��B	�B	�MB	��B	�B	�B	��B	�oB	�bB	��B	��B	�.B	��B	�bB	�B	��B	�B	�B	�B	�DB	�rB	�DB	�B	�VB	��B	�VB	��B	�"B	��B	��B	� B	��B	�.B	�oB	��B	�{B	��B	�B
 �B
 �B	��B	�YB	�eB
B
IB
CB	��B	��B
 rB
 rB
 �B
 =B	��B
 =B
 �B
B
 �B
�B
�B
�B
�B
�B
�B
VB
�B
�B
�B
.B
4B
�B
	@B

FB

�B

�B

�B
�B
�B
�B
�B
�B
$B
�B
0B
�B
*B
�B
�B
eB
B
=B
=B
�B
=B
B
CB
B
B
�B
B
}B
IB
IB
�B
�B
�B
!B
�B
'B
�B
�B
-B
�B
�B
hB
3B
B
�B
3B
nB
B
@B
nB
B
tB
tB
FB
tB
@B
�B
�B
�B
�B
�B
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
�B
�B
�B
*B
�B
!B
"<B
!kB
!�B
"qB
"�B
"<B
"<B
#BB
#wB
$}B
$�B
%�B
&�B
&�B
''B
'�B
'�B
)gB
)3B
)gB
)�B
*�B
*9B
*mB
+�B
*�B
*�B
+?B
+�B
,�B
,�B
,zB
-�B
.B
.B
-�B
-KB
-KB
-�B
.B
.B
.B
.RB
/�B
/�B
/XB
0�B
10B
1dB
2�B
26B
2�B
2�B
2�B
26B
3<B
3<B
2�B
3<B
4B
4�B
4BB
4BB
5HB
4�B
4�B
5B
5B
5}B
5B
5B
6NB
6B
5}B
5�B
6�B
6B
6B
6NB
7�B
7�B
8&B
8&B
8&B
8[B
8�B
8[B
8[B
8[B
8�B
8�B
9aB
9�B
:3B
;9B
:�B
;B
;9B
;9B
;mB
;�B
<?B
;�B
;mB
;9B
;B
:3B
:3B
:�B
;9B
;�B
;�B
<sB
<sB
<sB
=�B
=B
=�B
?B
@�B
A�B
A�B
B/B
C�B
CjB
BdB
A�B
A)B
@WB
@�B
@�B
A�B
B/B
EBB
H�B
H�B
HTB
I&B
I&B
H�B
H�B
HTB
I&B
I&B
J�B
KgB
J�B
J`B
K�B
LB
LmB
LmB
L�B
L�B
M
B
LmB
LmB
LmB
M
B
N�B
Q#B
QWB
QWB
QWB
Q�B
Q�B
R�B
Q�B
R)B
Q�B
R�B
R�B
S/B
ScB
TB
T�B
UB
UB
U;B
U�B
T�B
U�B
VB
V�B
VvB
WGB
VvB
W|B
WGB
W�B
XB
XB
W�B
W�B
X�B
X�B
YB
Y�B
ZZB
Z%B
Z�B
ZZB
Y�B
[,B
[`B
[`B
[�B
\2B
\�B
\2B
\�B
]B
\�B
]B
]�B
]�B
]�B
]lB
]�B
]�B
]�B
^rB
^rB
^
B
_B
^�B
^�B
_�B
_�B
`B
`B
_�B
`B
`JB
`B
`B
`B
`�B
a�B
aPB
aPB
a�B
bWB
bWB
bWB
c]B
c�B
c�B
c]B
c�B
d.B
dcB
dcB
dcB
d�B
d.B
d�B
d�B
d�B
d�B
e�B
fB
fB
fB
f;B
f;B
foB
f;B
f;B
f;B
e�B
f;B
gB
guB
gB
hB
hB
g�B
hB
g�B
hB
h{B
hB
h�B
iB
h�B
iB
iB
iB
i�B
jSB
jB
jB
j�B
j�B
k%B
kYB
k%B
kYB
k�B
k�B
l`B
l`B
l+B
l�B
l�B
l�B
m�B
n7B
nB
n�B
n�B
n�B
n�B
n�B
o�B
orB
o�B
o�B
o�B
o�B
o�B
o�B
orB
o�B
o�B
pB
pB
pxB
qB
q~B
p�B
qB
q�B
q�B
q~B
q�B
rB
r�B
r�B
r�B
r�B
r�B
s"B
s"B
r�B
s"B
s"B
sVB
s"B
s�B
s�B
t\B
t\B
t(B
t�B
t�B
t�B
ubB
u�B
u�B
u�B
v B
v B
v�B
v�B
v�B
v�B
v�B
v�B
wB
wB
wB
wB
wB
woB
w�B
xB
x@B
x�B
x�B
x�B
x�B
x�B
yB
x�B
yB
y�B
y�B
y�B
zMB
zMB
zMB
zMB
z�B
z�B
{B
z�B
{B
{B
{SB
{�B
{�B
|YB
|YB
|�B
|YB
}+B
}�B
~1B
}�B
~1B
~1B
~1B
~1B
~�B
~�B
~eB
~�B
~�B
~�B
~�B
7B
7B
7B
B
kB
�B
�B
�B
�B
�	B
�=B
�rB
�rB
��B
��B
�B
�B
�B
�B
�CB
�CB
�CB
��B
��B
��B
��B
��B
��B
��B
�B
�IB
�~B
�IB
�~B
�~B
��B
�B
�B
�B
�PB
�PB
��B
��B
��B
��B
��B
��B
��B
�!B
�!B
�VB
�!B
�VB
�VB
�VB
�!B
�!B
�VB
�VB
�!B
�!B
��B
�\B
�'B
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
�hB
��B
�B
�B
�:B
��B
�nB
�@B
�@B
�B
K�B
K�B
O�B
NyB
N�B
L�B
NEB
M>B
L�B
WB
M>B
IZB
P�B
K2B
J�B
J�B
L�B
OB
K�B
G�B
_yB
OB
K�B
IZB
K2B
I�B
I�B
I�B
J`B
I�B
I�B
I�B
I&B
I&B
I�B
H�B
G�B
G�B
G�B
G�B
IZB
H�B
IZB
I�B
I�B
I�B
I�B
I�B
H�B
I&B
H�B
H�B
H�B
HTB
HTB
HTB
H�B
H�B
H�B
I&B
H�B
IZB
IZB
J,B
J,B
J�B
J`B
J`B
J,B
I�B
I&B
I&B
H�B
HTB
H�B
I&B
J,B
I�B
J�B
J�B
J`B
I�B
I�B
IZB
IZB
IZB
H�B
H�B
H�B
I&B
H�B
H�B
I�B
J`B
J�B
J�B
J�B
J�B
J`B
J�B
I�B
I�B
IZB
IZB
I�B
J`B
J�B
J�B
K2B
I�B
I�B
I�B
IZB
I�B
J,B
K2B
KgB
LB
K�B
K2B
J�B
J�B
J`B
J,B
J�B
KgB
K�B
L�B
L�B
L8B
KgB
LB
LB
LB
L8B
KgB
K�B
LB
L�B
M�B
M�B
NEB
N�B
N�B
N�B
OB
OKB
OB
N�B
NyB
M�B
M�B
NyB
N�B
N�B
O�B
P�B
QWB
Q�B
Q�B
R]B
R]B
R�B
S�B
ScB
TB
T�B
VvB
VvB
WGB
WB
V�B
V�B
VAB
U�B
XNB
VB
W|B
VB
VAB
U�B
U�B
UpB
T�B
T�B
TB
TiB
ScB
S�B
VB
V�B
W�B
XB
XNB
Z%B
ZZB
X�B
YB
X�B
X�B
XB
X�B
X�B
X�B
XB
W�B
WGB
W|B
VAB
W�B
X�B
Y�B
Z�B
]8B
]B
^>B
_DB
_�B
`B
`�B
`�B
`�B
`�B
`JB
`B
`�B
aPB
b"B
c(B
c(B
e B
c�B
`�B
`B
aB
a�B
^�B
aPB
bWB
_�B
a�B
b�B
c�B
d�B
e B
fB
f;B
foB
e�B
f;B
e5B
gB
i�B
l�B
o�B
s�B
w�B
{�B
}�B
�B
�wB
��B
��B
��B
��B
��B
��B
��B
��B
�DB
�\B
�B
�7B$BBB$}B3�B4vB6B8&B:gB@�BE�BH�BK�BP�BVABW�BY�B[�B]B_�Bc�Bl�BqJBp�BpBp�Bq�BubBu�Bw�Bx�B~1B�'B��B�B�#B�jB��B�}B��B��B�EB��B�B�QB�QB��B�WB��B��BÞB�5B�jB�B�jB�B�BɏBǂB�ZB�&B��B˛B�8B�sB�B�NB�lB�B�.B��B�MB��B��B� B�bB��B�"B��B��B��B��B�{B��B$B"<BhB[B�B�BUB!B�B�B�B�B[BhB�B�B9B9BhB3B9BB�BnB�BnB�BtB�B�B-B�B!6B&�B!6B 0B*B#BBQ#BKgB?�B>�BA�BC5BE�BDpBF|BC�BC�BD�BIZBK�BJ�Bh�Bb�BhBq�By�B��B�PB�!B��B��B�~B�=B}�B|YB|�B{BxuBt�BxuBviBu.Bt(Bo>Bq�Bt�Bn�Bl�Bi�BhBiMBa�B_�B_B\�Bd�B��B<?B7TB0�B'�B!6BFBB�B�B �B��B�B��B�B��B��BB��Bv�BQ#BjBM�BR�BCB�BIB
��B =BFBVB
ӘB
�EB
װB
�|B
� B
ƱB
��B
�)B
�B
��B
��B
�aB
�B
��B
� B
�sB
��B
�HB
��B
�B
�<B
�0B
�B
��B
��B
��B
�EB
��B
��B
�B
�EB
��B
��B
�9B
�gB
�KB
��B
�aB
��B
��B
��B
��B
��B
� B
�B
��B
�'B
�0B
�}B
��B
�<B
�qB
��B
��B
��B
�UB
�FB
��B
��B
�*B
��B
��B
��B
�=B
�~B
�VB
��B
yB
|�B
�!B
mfB
�B
z�B
�IB
s�B
j�B
i�B
jB
k�B
o	B
iB
fB
d�B
fB
c�B
foB
f;B
sVB
eiB
f�B
e�B
g�B
g�B
i�B
p�B
l�B
e�B
guB
iB
j�B
pxB
j�B
k�B
l`B
l�B
j�B
h�B
gB
e�B
i�B
g�B
d.B
nlB
hB
d�B
e B
c(B
foB
_�B
^�B
]8B
\2B
]lB
_DB
Z�B
Z�B
W�B
YB
Z�B
Y�B
Z%B
X�B
VAB
YB
VvB
Y�B
WB
Q�B
O�B
L8B
L8B
S/B
]�B
NyB
G�B
-KB
,EB
)�B
"<B
-�B
-�B
LB
�B
kB
B
B
�B
kB
7B
7B
�B
�B
�B
kB
�B
RB
�B
�B
	@B

�B
nB
!B
\B
�B
�B
�B
�B
�B
4B
 	B
 rB	�7B	�eB
 =B	�%B	��B	�7B	�YB	�+B	��B	�B	��B	��B	�"B	�rB	�DB	��B	��B	�B	�7B	�7B	�+B	�+B	�B	��B	�{B	�VB	��B	�B	�`B	�B	�B	�uB	�uB	�B	�B	��B	�cB	�{B	�B	��B	�iB	�B	��B	�7B	�B	�B	��B	�SB	�B	��B	�B	�lB	��B	��B	�~B	�	B	��B	�iB	ާB	��B	�B	��B	�PB	�iB	�GB	�,B	�2B	�B	��B	�mB	̡B	�&B	�B	ȉB	ūB	�vB	ūB	��B	�jB	��B	�yB	�?B	�&B	�9B	έB	ϴB	��B	��B	� B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�}B	��B	��B	�\B	�B	x�B	x�B	woB	xuB	woB	x@B	u�B	u�B	y{B	x�B	xuB	s�B	q~B	pB	p�B	p�B	o>B	l`B	m�B	nB	n�B	i�B	jB	iMB	j�B	e�B	e�B	e�B	b�B	aB	a�B	d.B	yB	|%B	XB	R�B	P�B	M>B	H B	J�B	E�B	J�B	LB	G�B	BdB	H�B	@�B	A�B	EBB	H�B	A�B	A)B	=�B	=yB	@�B	<
B	9,B	;9B	;B	BdB	8�B	;�B	D<B	J,B	J`B	5B	.�B	4�B	1�B	0�B	.RB	1�B	/�B	0^B	.�B	.RB	.�B	-�B	.�B	.B	10B	.B	.�B	+�B	-B	,B	.�B	,�B	-B	+?B	*�B	)�B	*mB	,EB	*9B	*�B	(-B	(-B	(-B	(aB	%�B	*9B	%OB	$}B	$IB	$B	(-B	!�B	RB	'[B	"qB	�B	�B	OB	�B	eB	�B	�B	qB		�B	�B	 =B	�B��B��B�uB�B�oB��B��B�(B�bB��B�B�PB��B��B�	B��B��B�B�oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223235                            20230426223235AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622323520230426223235  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622323520230426223235QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622323520230426223235QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               