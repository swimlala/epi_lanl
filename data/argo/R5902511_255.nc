CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:49:57Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  P�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  U�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  jH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  op   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  �x   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` p   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T &�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   '$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ',   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   '4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   '<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 'D   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   '�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   '�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    '�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        (   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        (   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       (   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ( Argo profile    3.1 1.2 19500101000000  20230721224957  20230721224957  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�!��F3�@�!��F3�11  @�!���� @�!���� @2�%�ם
@2�%�ם
�d�V��d�V�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 BA  BA  FF  ?k�@   @E�@}p�@��R@�  @�  A   A  A\)A+�A?\)A`  A�  A�Q�A�Q�A�Q�A�  A�  A�  A�Q�A��B  B  B  B   B(  B/�
B8  B@(�BH  BO�
BX(�B`(�Bh(�Bp(�Bx  B�  B�  B�  B�(�B�{B�  B�(�B�{B�  B�=qB�(�B��
B��
B�  B��B��B�  B�(�B�(�B�{B��B�  B�  B�  B�  B�  B��B��B�  B�{B�{B��
C   C  C
=C
=C  C
  C  C  C  C
=C{C
=C  C��C�C�C   C"
=C$
=C&
=C(
=C)��C+��C.
=C0
=C2  C3��C5��C7��C9��C;��C>
=C@  CA��CC��CE��CH  CI��CK��CM��CO��CQ��CT  CV  CX  CZ
=C\  C^  C`  Ca��Cd  Cf
=Ch  Cj  Cl  Cm��Cp  Cr  Cs��Cv  Cx  Cy�C{�C}�C��C�  C���C�  C�  C�  C�  C�  C�  C���C���C���C�  C�  C���C�  C�  C�  C�  C�C�C�C���C���C���C���C�  C���C���C���C���C��C���C�C�  C�  C�C�C���C�  C�C���C�  C�  C���C���C�  C�C���C�  C�  C���C�  C�  C�C�C���C���C���C�  C�C�C�  C�  C�  C���C���C�C�
=C�
=C�  C�C�  C���C��C���C�  C�C�  C���C�  C�  C�  C�  C���C���C���C�  C���C���C��C���C�  C���C�C�C�  C�  C�  C���C���C���C���C���C���C�  C�C�  C���C���C�  C���C��C�  C�C���C�  C�  C�  C���C��C���C���C���C�  C�C���C���C���D � D  D}qD�qD� D  D}qD  D��D�D�D�D}qD��D��D  Dz�D	  D	��D
�D
� D
�qD}qD�D��D  D� D�D��D  D� D  D� D  D��DD� D�qD� D  D}qD  D� D�qD� D  D��D  D��DD��D  D��D  D��D�D� D�qDz�D  D��D�D� D   D ��D!�D!��D"�D"��D#�D#�D#�qD$z�D$�qD%}qD&  D&� D&��D'}qD'�qD(� D)�D)� D)�qD*z�D*��D+z�D,  D,� D-  D-� D-�qD.}qD/  D/��D0�D0� D1  D1}qD1�qD2� D3  D3� D4�D4��D5�D5}qD5�RD6z�D7  D7� D7�qD8��D9D9}qD:  D:��D;  D;}qD;�qD<}qD<�qD=� D=�qD>}qD>�qD?��D@  D@z�DA  DA��DB  DB}qDB�qDC� DD  DD��DEDE��DE��DF� DGDG� DG�qDH� DI  DI� DJ  DJ��DK  DK��DLDL��DL�qDMz�DM�qDN��DO  DO� DP  DP� DQDQ��DR�DR}qDR��DS� DT�DT� DT��DU� DV�DV��DW�DW�DX�DX� DY  DY� DZ  DZ� D[  D[� D\�D\� D]  D]}qD]��D^}qD^�qD_z�D_�qD`}qDa  Da}qDb  Db}qDb�qDcz�Dc��Dd� De�De}qDe��Df}qDg  Dg� Dg�qDh}qDh��Di}qDj  Dj� Dj�qDk}qDk�qDl� Dm�Dm�Dn  Dn}qDo  Do�Dp�Dp� Dq  Dq� Dr�Dr��Ds  Ds}qDt  Dt� Du  Du��Dv�Dv� Dw  Dw��Dx�Dx�Dy�Dy� Dy�qDzz�Dz�qD{� D{�qD|}qD}  D}��D~�D~��D~�qD� D�HD�@ D��HD�� D��)D�>�D��HD���D��qD�>�D�~�D���D�HD�@ D��HD��HD���D�>�D��HD�� D�  D�AHD��HD���D�  D�B�D��HD�� D���D�>�D�� D���D���D�@ D��HD�� D���D�=qD�� D��HD�HD�@ D�~�D���D���D�>�D�~�D�� D�  D�@ D��HD�D�HD�AHD��HD�� D�HD�@ D�� D�� D���D�>�D�~�D�� D�  D�AHD��HD���D�  D�>�D�~�D�� D�  D�>�D�� D��HD�  D�>�D�� D��HD�HD�@ D��HD��HD�  D�@ D�� D�� D�HD�@ D�~�D���D���D�@ D�� D�� D�  D�ED�b�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?B�\?�  ?�\)?\?��@�\@\)@�R@8Q�@E�@\(�@p��@�  @��@�z�@�(�@��@��@�p�@���@��@޸R@���@��@���A�
AffA��A�AA�A!G�A%�A+�A0  A4z�A:�HA?\)AC�
AJ�HAO\)AS33AY��A^{AaG�Ag�Al��Ap��AuA|(�A�  A�=qA�p�A��A���A���A�
=A�G�A�(�A�
=A���A��A�{A�  A��\A�p�A�\)A��A�z�A�ffA���A��A�A�\)A��A�z�A�{A���A��HA��AǮAə�A˅A�{A�Q�A��A�z�A�
=A�Q�A��HA�p�A�
=A���A��
A�p�A�\)A�=qA�z�A�ffA���A�33A��A��RA���A�33A��A�\)B ��BG�B�\B�B��BG�BffB�BQ�B	G�B
�RB�BQ�Bp�B�RB\)Bz�B�B�RB�B��B{B�HB�
BG�B=qB
=B  Bp�B=qB�RB   B!G�B!�B"�RB$  B%�B%��B&�HB((�B(��B)B+
=B,  B,��B-B/33B0(�B0��B2{B3\)B4  B4��B6=qB7\)B8(�B9�B:�\B;�B<Q�B=��B>�RB?�B@��BA�BB�RBC�BD��BF{BF�HBG�
BI�BJ=qBJ�HBL(�BMG�BM�BN�HBP(�BQ�BQBS33BTQ�BT��BV{BW\)BX  BX��BZffB[33B\  B]�B^=qB_33B`  Ba�BbffBc\)Bd(�BeG�Bf�RBg�Bhz�Bi�Bk
=Bl  Bl��Bn{Bo�BpQ�BqG�Br�\Bs�
Bt��Bu�Bw\)BxQ�ByG�BzffB{�
B|��B}�B~�HB�(�B��HB�33B��
B��\B�G�B�B�=qB��HB��B�(�B���B��B��B��\B�
=B���B�Q�B�
=B��B�  B���B��B��B�z�B�33B��
B�Q�B��HB���B�=qB��RB�33B��B��\B�
=B���B�=qB�
=B��B�  B���B��B��B���B�p�B�  B�ffB��B��B�ffB���B�B�ffB���B��B�=qB�
=B��B�{B���B��B�  B���B�p�B��B�z�B�G�B��
B�ffB��B��
B�ffB��HB��B�ffB���B���B�Q�B��RB�p�B�=qB���B�\)B�{B���B��B��B��\B�
=B�B�z�B���B���B�ffB���B�\)B�(�B��RB�33B��B��\B���B��B�ffB�
=B�p�B�(�B���B�p�B��Bď\B�G�B�B�ffB��BǙ�B�{B��HB�\)B��
Bʏ\B�33BˮB�=qB���B͙�B�  BΣ�B�p�B�  B�z�B�33B��B�ffB���Bә�B�ffB�
=BՅB�(�B���Bי�B�{Bأ�BمB�(�Bڣ�B�\)B�{BܸRB��B�Bޏ\B�G�B߮B�=qB��HBᙚB�=qB��B�\)B�  B��B�33B�B�ffB��B�B�{B�RB�p�B�{B�\B��B�B�ffB�
=B�B�{B���B�p�B��B�z�B�33B��B�Q�B��HB�B�(�B���B��B�B�ffB�
=B��B��B�z�B��B�B�(�B���B�\)B�  B�z�B���B��B�=qB��HB�p�B��C =qC �\C �C33Cz�C�RC
=CffC�C�C33Cz�C�
C(�C\)C��C  C\)C��C�C33Cp�CC�Cz�C��C�C\)C�C	
=C	\)C	�RC

=C
Q�C
��C
�HC=qC��C�C(�Cp�CC{CffC�RC
=CQ�C�\C�HC=qC�\C�HC33Cz�C�RC
=CQ�C��C  CQ�C�\C�
C�Cp�C��C(�Cp�C�C��C=qC��C�CQ�C��C��CQ�C��C��C=qC�C��C(�C�\C�CG�C�\C�
C(�C�C�HC33C�C�HC(�Cp�CC{CffC�RC
=C\)C�RC �C p�C ��C!�C!p�C!C"
=C"\)C"�C"��C#Q�C#��C$
=C$p�C$��C%(�C%z�C%��C&�C&z�C&��C'(�C'�C'�HC(33C(�\C(�HC)=qC)��C)��C*Q�C*�C+  C+G�C+��C+��C,Q�C,��C,��C-=qC-��C-��C.G�C.��C.��C/Q�C/�C0
=C0ffC0�RC1{C1ffC1C2�C2p�C2C3{C3p�C3��C4�C4z�C4�HC5=qC5��C5��C6Q�C6�C7
=C7ffC7C8�C8z�C8�HC933C9��C9��C:\)C:�RC;{C;p�C;C<�C<z�C<�HC==qC=�\C=�C>Q�C>��C>��C?Q�C?�C@  C@\)C@�CA  CA\)CA�CB
=CB\)CB�RCC{CCp�CC��CD�CDz�CD�
CE(�CE�CE�HCF33CF�\CF�CG=qCG��CG��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111133333333333                                            111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?k�@   @E�@}p�@��R@�  @�  A   A  A\)A+�A?\)A`  A�  A�Q�A�Q�A�Q�A�  A�  A�  A�Q�A��B  B  B  B   B(  B/�
B8  B@(�BH  BO�
BX(�B`(�Bh(�Bp(�Bx  B�  B�  B�  B�(�B�{B�  B�(�B�{B�  B�=qB�(�B��
B��
B�  B��B��B�  B�(�B�(�B�{B��B�  B�  B�  B�  B�  B��B��B�  B�{B�{B��
C   C  C
=C
=C  C
  C  C  C  C
=C{C
=C  C��C�C�C   C"
=C$
=C&
=C(
=C)��C+��C.
=C0
=C2  C3��C5��C7��C9��C;��C>
=C@  CA��CC��CE��CH  CI��CK��CM��CO��CQ��CT  CV  CX  CZ
=C\  C^  C`  Ca��Cd  Cf
=Ch  Cj  Cl  Cm��Cp  Cr  Cs��Cv  Cx  Cy�C{�C}�C��C�  C���C�  C�  C�  C�  C�  C�  C���C���C���C�  C�  C���C�  C�  C�  C�  C�C�C�C���C���C���C���C�  C���C���C���C���C��C���C�C�  C�  C�C�C���C�  C�C���C�  C�  C���C���C�  C�C���C�  C�  C���C�  C�  C�C�C���C���C���C�  C�C�C�  C�  C�  C���C���C�C�
=C�
=C�  C�C�  C���C��C���C�  C�C�  C���C�  C�  C�  C�  C���C���C���C�  C���C���C��C���C�  C���C�C�C�  C�  C�  C���C���C���C���C���C���C�  C�C�  C���C���C�  C���C��C�  C�C���C�  C�  C�  C���C��C���C���C���C�  C�C���C���C���D � D  D}qD�qD� D  D}qD  D��D�D�D�D}qD��D��D  Dz�D	  D	��D
�D
� D
�qD}qD�D��D  D� D�D��D  D� D  D� D  D��DD� D�qD� D  D}qD  D� D�qD� D  D��D  D��DD��D  D��D  D��D�D� D�qDz�D  D��D�D� D   D ��D!�D!��D"�D"��D#�D#�D#�qD$z�D$�qD%}qD&  D&� D&��D'}qD'�qD(� D)�D)� D)�qD*z�D*��D+z�D,  D,� D-  D-� D-�qD.}qD/  D/��D0�D0� D1  D1}qD1�qD2� D3  D3� D4�D4��D5�D5}qD5�RD6z�D7  D7� D7�qD8��D9D9}qD:  D:��D;  D;}qD;�qD<}qD<�qD=� D=�qD>}qD>�qD?��D@  D@z�DA  DA��DB  DB}qDB�qDC� DD  DD��DEDE��DE��DF� DGDG� DG�qDH� DI  DI� DJ  DJ��DK  DK��DLDL��DL�qDMz�DM�qDN��DO  DO� DP  DP� DQDQ��DR�DR}qDR��DS� DT�DT� DT��DU� DV�DV��DW�DW�DX�DX� DY  DY� DZ  DZ� D[  D[� D\�D\� D]  D]}qD]��D^}qD^�qD_z�D_�qD`}qDa  Da}qDb  Db}qDb�qDcz�Dc��Dd� De�De}qDe��Df}qDg  Dg� Dg�qDh}qDh��Di}qDj  Dj� Dj�qDk}qDk�qDl� Dm�Dm�Dn  Dn}qDo  Do�Dp�Dp� Dq  Dq� Dr�Dr��Ds  Ds}qDt  Dt� Du  Du��Dv�Dv� Dw  Dw��Dx�Dx�Dy�Dy� Dy�qDzz�Dz�qD{� D{�qD|}qD}  D}��D~�D~��D~�qD� D�HD�@ D��HD�� D��)D�>�D��HD���D��qD�>�D�~�D���D�HD�@ D��HD��HD���D�>�D��HD�� D�  D�AHD��HD���D�  D�B�D��HD�� D���D�>�D�� D���D���D�@ D��HD�� D���D�=qD�� D��HD�HD�@ D�~�D���D���D�>�D�~�D�� D�  D�@ D��HD�D�HD�AHD��HD�� D�HD�@ D�� D�� D���D�>�D�~�D�� D�  D�AHD��HD���D�  D�>�D�~�D�� D�  D�>�D�� D��HD�  D�>�D�� D��HD�HD�@ D��HD��HD�  D�@ D�� D�� D�HD�@ D�~�D���D���D�@ D�� D�� D�  D�ED�b�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?B�\?�  ?�\)?\?��@�\@\)@�R@8Q�@E�@\(�@p��@�  @��@�z�@�(�@��@��@�p�@���@��@޸R@���@��@���A�
AffA��A�AA�A!G�A%�A+�A0  A4z�A:�HA?\)AC�
AJ�HAO\)AS33AY��A^{AaG�Ag�Al��Ap��AuA|(�A�  A�=qA�p�A��A���A���A�
=A�G�A�(�A�
=A���A��A�{A�  A��\A�p�A�\)A��A�z�A�ffA���A��A�A�\)A��A�z�A�{A���A��HA��AǮAə�A˅A�{A�Q�A��A�z�A�
=A�Q�A��HA�p�A�
=A���A��
A�p�A�\)A�=qA�z�A�ffA���A�33A��A��RA���A�33A��A�\)B ��BG�B�\B�B��BG�BffB�BQ�B	G�B
�RB�BQ�Bp�B�RB\)Bz�B�B�RB�B��B{B�HB�
BG�B=qB
=B  Bp�B=qB�RB   B!G�B!�B"�RB$  B%�B%��B&�HB((�B(��B)B+
=B,  B,��B-B/33B0(�B0��B2{B3\)B4  B4��B6=qB7\)B8(�B9�B:�\B;�B<Q�B=��B>�RB?�B@��BA�BB�RBC�BD��BF{BF�HBG�
BI�BJ=qBJ�HBL(�BMG�BM�BN�HBP(�BQ�BQBS33BTQ�BT��BV{BW\)BX  BX��BZffB[33B\  B]�B^=qB_33B`  Ba�BbffBc\)Bd(�BeG�Bf�RBg�Bhz�Bi�Bk
=Bl  Bl��Bn{Bo�BpQ�BqG�Br�\Bs�
Bt��Bu�Bw\)BxQ�ByG�BzffB{�
B|��B}�B~�HB�(�B��HB�33B��
B��\B�G�B�B�=qB��HB��B�(�B���B��B��B��\B�
=B���B�Q�B�
=B��B�  B���B��B��B�z�B�33B��
B�Q�B��HB���B�=qB��RB�33B��B��\B�
=B���B�=qB�
=B��B�  B���B��B��B���B�p�B�  B�ffB��B��B�ffB���B�B�ffB���B��B�=qB�
=B��B�{B���B��B�  B���B�p�B��B�z�B�G�B��
B�ffB��B��
B�ffB��HB��B�ffB���B���B�Q�B��RB�p�B�=qB���B�\)B�{B���B��B��B��\B�
=B�B�z�B���B���B�ffB���B�\)B�(�B��RB�33B��B��\B���B��B�ffB�
=B�p�B�(�B���B�p�B��Bď\B�G�B�B�ffB��BǙ�B�{B��HB�\)B��
Bʏ\B�33BˮB�=qB���B͙�B�  BΣ�B�p�B�  B�z�B�33B��B�ffB���Bә�B�ffB�
=BՅB�(�B���Bי�B�{Bأ�BمB�(�Bڣ�B�\)B�{BܸRB��B�Bޏ\B�G�B߮B�=qB��HBᙚB�=qB��B�\)B�  B��B�33B�B�ffB��B�B�{B�RB�p�B�{B�\B��B�B�ffB�
=B�B�{B���B�p�B��B�z�B�33B��B�Q�B��HB�B�(�B���B��B�B�ffB�
=B��B��B�z�B��B�B�(�B���B�\)B�  B�z�B���B��B�=qB��HB�p�B��C =qC �\C �C33Cz�C�RC
=CffC�C�C33Cz�C�
C(�C\)C��C  C\)C��C�C33Cp�CC�Cz�C��C�C\)C�C	
=C	\)C	�RC

=C
Q�C
��C
�HC=qC��C�C(�Cp�CC{CffC�RC
=CQ�C�\C�HC=qC�\C�HC33Cz�C�RC
=CQ�C��C  CQ�C�\C�
C�Cp�C��C(�Cp�C�C��C=qC��C�CQ�C��C��CQ�C��C��C=qC�C��C(�C�\C�CG�C�\C�
C(�C�C�HC33C�C�HC(�Cp�CC{CffC�RC
=C\)C�RC �C p�C ��C!�C!p�C!C"
=C"\)C"�C"��C#Q�C#��C$
=C$p�C$��C%(�C%z�C%��C&�C&z�C&��C'(�C'�C'�HC(33C(�\C(�HC)=qC)��C)��C*Q�C*�C+  C+G�C+��C+��C,Q�C,��C,��C-=qC-��C-��C.G�C.��C.��C/Q�C/�C0
=C0ffC0�RC1{C1ffC1C2�C2p�C2C3{C3p�C3��C4�C4z�C4�HC5=qC5��C5��C6Q�C6�C7
=C7ffC7C8�C8z�C8�HC933C9��C9��C:\)C:�RC;{C;p�C;C<�C<z�C<�HC==qC=�\C=�C>Q�C>��C>��C?Q�C?�C@  C@\)C@�CA  CA\)CA�CB
=CB\)CB�RCC{CCp�CC��CD�CDz�CD�
CE(�CE�CE�HCF33CF�\CF�CG=qCG��CG��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111133333333333                                            111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ƨA���A���A���A��
A��
A��
A���A��A��A��A��A��A��A��#A��A��#A��#A��A�AήAΑhA�7LA�M�A�l�A��Aˏ\A�p�A�ȴA�G�A�7LA��AǾwAǁA�VA��A���A���AƍPAƋDA���A���Aơ�A��A�=qA�r�A�t�A��7A���A���A�?}A�
=A���A��HA�bNA��;A��FA�dZA��A�(�A��;A�7LA�A�ĜA��A��`A���A�~�A�ZA��\A��/A�9XA���A�ƨA�33A�A�1A�{A��FA��9A��A��A���A�E�A��;A��7A�XA��yA�
=A}��A|�jAz�Ay��AvffAs��ApZAo33An-Aj�9AhȴAg�mAf�Aet�AcƨAb-A^�uAY�AW+AVI�AT��ARĜAOG�AN�ANVAN�AM��AM�7AM�AM�AJ�/AEA?�A>JA<��A<^5A;S�A: �A89XA7�A7��A7S�A4JA0z�A.M�A-��A,ĜA*��A)�^A)VA(��A(�uA'A$�!A#�TA#�7A"ZA!oA�A�;A+At�AZAI�AVA�;A;dA�A�A�A�#A5?A1'A��A��A1AAx�AA��AA�A=qAE�A1'A�A1'AK�A�`A�AbAA(�A�!A�RA1A��A^5A��A
VA	&�A�TA��A~�A�mA�A��A��AZA ��@��!@���@��+@���@�j@�@�+@��@���@�+@�u@�v�@�O�@�D@�  @�"�@��@�E�@��@��@�-@��@���@�7@�@߾w@���@�~�@�$�@�=q@ܬ@�C�@�ȴ@�^5@ڟ�@���@�9X@��@�M�@�=q@��T@թ�@�j@�33@��@җ�@��@�`B@��`@�Q�@�(�@�ƨ@�dZ@�C�@�;d@�33@�;d@��@��H@��@�/@̓u@�(�@�ƨ@�@��@ʰ!@�J@�O�@���@� �@�|�@Ə\@�{@ŉ7@ũ�@�X@ēu@�@�33@�A�@ɉ7@�@�7L@ȴ9@���@ǍP@�+@��y@��@���@Ɵ�@ź^@�/@���@ě�@�r�@�r�@�Z@��@�l�@�@�@�hs@��7@�@�7L@�z�@��@���@���@���@�G�@�&�@�7L@��T@���@��9@�bN@�I�@�Z@��u@�z�@�Q�@�I�@�b@��w@�t�@��@��@���@�n�@�5?@��@��7@�?}@���@���@��9@�Z@��@��F@��P@�K�@��@�ȴ@���@�5?@���@�`B@�/@���@�Ĝ@�z�@�I�@��@��P@�S�@��@���@�=q@��@��-@�V@��/@���@�bN@��@�o@���@�~�@�v�@�v�@�J@�X@���@�(�@��
@�dZ@�;d@�o@��y@��R@���@�^5@���@�%@�Q�@�  @�ƨ@�S�@���@�=q@��@��T@��#@���@��h@�hs@�?}@��@��u@�Z@��m@��@�\)@���@���@�ff@�$�@�@���@�hs@��@��`@��D@�z�@�j@�(�@��m@�S�@���@��\@�@���@���@��j@��D@�A�@�  @��
@�\)@���@�E�@�`B@��j@�Q�@���@��@���@��!@��+@�V@�=q@�$�@�J@��#@��^@�?}@��j@�Q�@��w@�\)@��@�
=@�~�@��^@�/@���@���@���@�I�@��F@�dZ@�K�@�C�@�
=@��y@���@�=q@�$�@��@�{@�{@�@���@���@���@�9X@�b@���@�l�@�K�@�33@��@���@���@���@���@��\@��\@��+@�v�@�ff@�ff@�V@�5?@�J@��@��-@���@�G�@��@�(�@�  @��@�|�@�S�@�33@�"�@�o@���@���@�v�@�ff@�J@�7L@�&�@��@��@��`@�Z@�9X@�  @��F@���@���@���@���@�l�@�\)@�+@�"�@�"�@��@�
=@�@��@���@��@��@��^@���@���@�hs@��@���@��`@��/@���@�Ĝ@�Ĝ@��j@��j@��9@��@��D@�z�@�r�@�bN@�Q�@��@~��@~E�@}O�@|�@|(�@|1@|1@|�@{��@z-@yx�@x�9@xA�@w�P@vE�@u@u�-@u�@u/@t��@t�@tz�@t1@s�m@s�m@s�
@sƨ@s�F@s��@sdZ@s33@s"�@s@r��@rn�@q��@qG�@q�@p��@pQ�@p  @o��@oK�@o
=@n�R@n�+@n5?@m�@m@m��@m�@m`B@m�@l�@l�j@l��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AξwA���A�ȴA�ƨA���A�ĜA���A���A���A��
A���A���A��A���A���A��
A���A��A���A���A��A���A���A��A��
A���A��A���A��A��#A���A��A��#A���A��#A��A��
A��#A��A��
A��#A��A��
A��#A��#A���A��#A��/A��
A��A��/A��A��A��/A��A��A��/A��#A��A��#A��#A��
A��#A��#A���A��A��A��
A��A��/A��#A��#A��/A��/A��A��/A��/A��
A��#A��A��#A��/A��A��
A��/A��/A��
A���A��
A���A�ĜA�ƨA�ĜAξwA�AμjAξwAμjAδ9AΩ�AΧ�AήAΟ�AΥ�AήAΝ�A΃A΃A΃AΗ�A΁A΍PA΁A�XA�I�A�?}A�&�A�(�A��A��A�ȴA�ĜA͇+A�~�A�bNA��A�ƨẠ�A̓uÃA�l�A�ffA�^5A�XA�M�A�G�A�=qA�1'A�&�A��A��A�VA���A���A��mA��A�ĜA˩�A˙�A�v�A��`A�C�Aɴ9A�n�A�ZA�Q�A�A�A�;dA�9XA�1'A�-A�/A� �A���A�`BA�VA�O�A�M�A�O�A�M�A�C�A�E�A�G�A�C�A�=qA�=qA�=qA�7LA�5?A�7LA�7LA�1'A�1'A�33A� �A��A� �A��A�JA��A��`A��
A���A���A�ĜAǺ^Aǩ�Aǟ�AǛ�Aǝ�Aǝ�AǏ\A�~�A�t�A�l�A�bNA�\)A�^5A�`BA�ZA�XA�S�A�M�A�7LA��A��A��A�oA�oA�{A�bA�A�A�A���A���A��A��A��`A���A���A���A���AƼjAƣ�AƝ�Aƛ�AƓuAƇ+A�z�A�t�A�~�A�z�A�z�AƃAƏ\Aƕ�Aƣ�AƶFA���A���A���A��
A��A��A��A��/A��HA��#A���A���Aư!AƟ�Aƛ�Aƥ�AƟ�AƝ�AƝ�Aƕ�AƍPA�jA�/A���AŶFAś�AŋDA�v�A�/A�VA�1A�1A���A��A��`AčPA��A�n�A���A¬A§�A�A�v�A��A�ȴA��\A�v�A��!A��FA�ZA�S�A�;dA� �A�1A���A�dZA��TA�?}A���A�dZA�bNA�^5A�`BA�ZA�O�A�K�A�I�A�=qA�"�A� �A��A�bA�JA�1A���A���A���A���A���A���A���A��A���A��A��A��A��HA���A�A��FA���A��A�jA�M�A�1'A�{A�1A�  A��A��`A��
A���A���A�Q�A���A���A���A��uA�E�A��TA�bNA���A� �A��mA��A�1'A���A�"�A��A��\A��DA�z�A�G�A�A��;A�33A�1'A���A�ȴA�VA��+A�|�A�x�A�l�A�ffA�ffA�bNA�XA�9XA��A��A��\A�^5A�{A���A�^5A�?}A��A��wA�O�A��PA�bNA�I�A�33A��A�
=A���A��TA�A��A���A��uA�~�A�p�A�C�A��A�
=A�33A�I�A��yA�A��RA��A���A���A�x�A�  A��!A�S�A�"�A�oA��A�A�n�A�5?A���A�
=A�hsA�1A��!A��HA��A�\)A�I�A�5?A��A���A���A��`A��/A���A���A���A��\A�jA�`BA�O�A�&�A��mA���A�ffA�O�A���A�$�A�ĜA�dZA���A�-A�A��DA�jA�`BA�S�A�I�A�?}A�9XA�+A��A�bA�JA��A�ƨA��9A���A��7A�^5A�K�A�5?A��A���A��9A��A�dZA�C�A��A�JA���A�ȴA�ffA�33A�&�A���A��!A�I�A�A��\A�G�A���A��PA��A��A�  A��+A�(�A��9A�bNA��;A��hA�oA���A��A��DA�jA�ZA�(�A�A��A��A��/A���A���A��A�bNA�JA���A��DA�bNA�+A�E�A�;dA�ȴA�hsA�-A��yA�r�A��-A�{A�ȴA���A��PA�|�A�bNA�A�A��A��A�ƨA��DA���A�G�A�oA��A�E�A�&�A�AƨAXA~�HA~�A}�A}|�A}O�A};dA}�A}A|�A|��A|5?A{�Azv�Az�\Az�DAz��Az��Az��Az��Az�Az��Az1Ax��Ax�Aw�Av�yAvM�Au�FAu"�At�9AtQ�As�AsAs�As+Ar�`Aq%Ao��Ao��Ao�PAo\)AoXAoG�Ao/Ao"�AooAo
=An�/An�DAn�Am�7Al�HAlbAkO�Ajn�Ai�Ai��AiO�Ai�AhȴAh��Ah�DAhv�AhVAh{Ag�#Ag�wAg��Ag�Agt�Ag\)Ag&�Afv�Af=qAf1'Ae�AeC�AedZAeC�AeAd��Ac��Ac��Ac�Acl�Ac`BAcO�Ab�AbffAb{Aa"�A`VA_�mA_�A^�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111133333333333                                            111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ƨA���A���A���A��
A��
A��
A���A��A��A��A��A��A��A��#A��A��#A��#A��A�AήAΑhA�7LA�M�A�l�A��Aˏ\A�p�A�ȴA�G�A�7LA��AǾwAǁA�VA��A���A���AƍPAƋDA���A���Aơ�A��A�=qA�r�A�t�A��7A���A���A�?}A�
=A���A��HA�bNA��;A��FA�dZA��A�(�A��;A�7LA�A�ĜA��A��`A���A�~�A�ZA��\A��/A�9XA���A�ƨA�33A�A�1A�{A��FA��9A��A��A���A�E�A��;A��7A�XA��yA�
=A}��A|�jAz�Ay��AvffAs��ApZAo33An-Aj�9AhȴAg�mAf�Aet�AcƨAb-A^�uAY�AW+AVI�AT��ARĜAOG�AN�ANVAN�AM��AM�7AM�AM�AJ�/AEA?�A>JA<��A<^5A;S�A: �A89XA7�A7��A7S�A4JA0z�A.M�A-��A,ĜA*��A)�^A)VA(��A(�uA'A$�!A#�TA#�7A"ZA!oA�A�;A+At�AZAI�AVA�;A;dA�A�A�A�#A5?A1'A��A��A1AAx�AA��AA�A=qAE�A1'A�A1'AK�A�`A�AbAA(�A�!A�RA1A��A^5A��A
VA	&�A�TA��A~�A�mA�A��A��AZA ��@��!@���@��+@���@�j@�@�+@��@���@�+@�u@�v�@�O�@�D@�  @�"�@��@�E�@��@��@�-@��@���@�7@�@߾w@���@�~�@�$�@�=q@ܬ@�C�@�ȴ@�^5@ڟ�@���@�9X@��@�M�@�=q@��T@թ�@�j@�33@��@җ�@��@�`B@��`@�Q�@�(�@�ƨ@�dZ@�C�@�;d@�33@�;d@��@��H@��@�/@̓u@�(�@�ƨ@�@��@ʰ!@�J@�O�@���@� �@�|�@Ə\@�{@ŉ7@ũ�@�X@ēu@�@�33@�A�@ɉ7@�@�7L@ȴ9@���@ǍP@�+@��y@��@���@Ɵ�@ź^@�/@���@ě�@�r�@�r�@�Z@��@�l�@�@�@�hs@��7@�@�7L@�z�@��@���@���@���@�G�@�&�@�7L@��T@���@��9@�bN@�I�@�Z@��u@�z�@�Q�@�I�@�b@��w@�t�@��@��@���@�n�@�5?@��@��7@�?}@���@���@��9@�Z@��@��F@��P@�K�@��@�ȴ@���@�5?@���@�`B@�/@���@�Ĝ@�z�@�I�@��@��P@�S�@��@���@�=q@��@��-@�V@��/@���@�bN@��@�o@���@�~�@�v�@�v�@�J@�X@���@�(�@��
@�dZ@�;d@�o@��y@��R@���@�^5@���@�%@�Q�@�  @�ƨ@�S�@���@�=q@��@��T@��#@���@��h@�hs@�?}@��@��u@�Z@��m@��@�\)@���@���@�ff@�$�@�@���@�hs@��@��`@��D@�z�@�j@�(�@��m@�S�@���@��\@�@���@���@��j@��D@�A�@�  @��
@�\)@���@�E�@�`B@��j@�Q�@���@��@���@��!@��+@�V@�=q@�$�@�J@��#@��^@�?}@��j@�Q�@��w@�\)@��@�
=@�~�@��^@�/@���@���@���@�I�@��F@�dZ@�K�@�C�@�
=@��y@���@�=q@�$�@��@�{@�{@�@���@���@���@�9X@�b@���@�l�@�K�@�33@��@���@���@���@���@��\@��\@��+@�v�@�ff@�ff@�V@�5?@�J@��@��-@���@�G�@��@�(�@�  @��@�|�@�S�@�33@�"�@�o@���@���@�v�@�ff@�J@�7L@�&�@��@��@��`@�Z@�9X@�  @��F@���@���@���@���@�l�@�\)@�+@�"�@�"�@��@�
=@�@��@���@��@��@��^@���@���@�hs@��@���@��`@��/@���@�Ĝ@�Ĝ@��j@��j@��9@��@��D@�z�@�r�@�bN@�Q�@��@~��@~E�@}O�@|�@|(�@|1@|1@|�@{��@z-@yx�@x�9@xA�@w�P@vE�@u@u�-@u�@u/@t��@t�@tz�@t1@s�m@s�m@s�
@sƨ@s�F@s��@sdZ@s33@s"�@s@r��@rn�@q��@qG�@q�@p��@pQ�@p  @o��@oK�@o
=@n�R@n�+@n5?@m�@m@m��@m�@m`B@m�@l�@l�j@l��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AξwA���A�ȴA�ƨA���A�ĜA���A���A���A��
A���A���A��A���A���A��
A���A��A���A���A��A���A���A��A��
A���A��A���A��A��#A���A��A��#A���A��#A��A��
A��#A��A��
A��#A��A��
A��#A��#A���A��#A��/A��
A��A��/A��A��A��/A��A��A��/A��#A��A��#A��#A��
A��#A��#A���A��A��A��
A��A��/A��#A��#A��/A��/A��A��/A��/A��
A��#A��A��#A��/A��A��
A��/A��/A��
A���A��
A���A�ĜA�ƨA�ĜAξwA�AμjAξwAμjAδ9AΩ�AΧ�AήAΟ�AΥ�AήAΝ�A΃A΃A΃AΗ�A΁A΍PA΁A�XA�I�A�?}A�&�A�(�A��A��A�ȴA�ĜA͇+A�~�A�bNA��A�ƨẠ�A̓uÃA�l�A�ffA�^5A�XA�M�A�G�A�=qA�1'A�&�A��A��A�VA���A���A��mA��A�ĜA˩�A˙�A�v�A��`A�C�Aɴ9A�n�A�ZA�Q�A�A�A�;dA�9XA�1'A�-A�/A� �A���A�`BA�VA�O�A�M�A�O�A�M�A�C�A�E�A�G�A�C�A�=qA�=qA�=qA�7LA�5?A�7LA�7LA�1'A�1'A�33A� �A��A� �A��A�JA��A��`A��
A���A���A�ĜAǺ^Aǩ�Aǟ�AǛ�Aǝ�Aǝ�AǏ\A�~�A�t�A�l�A�bNA�\)A�^5A�`BA�ZA�XA�S�A�M�A�7LA��A��A��A�oA�oA�{A�bA�A�A�A���A���A��A��A��`A���A���A���A���AƼjAƣ�AƝ�Aƛ�AƓuAƇ+A�z�A�t�A�~�A�z�A�z�AƃAƏ\Aƕ�Aƣ�AƶFA���A���A���A��
A��A��A��A��/A��HA��#A���A���Aư!AƟ�Aƛ�Aƥ�AƟ�AƝ�AƝ�Aƕ�AƍPA�jA�/A���AŶFAś�AŋDA�v�A�/A�VA�1A�1A���A��A��`AčPA��A�n�A���A¬A§�A�A�v�A��A�ȴA��\A�v�A��!A��FA�ZA�S�A�;dA� �A�1A���A�dZA��TA�?}A���A�dZA�bNA�^5A�`BA�ZA�O�A�K�A�I�A�=qA�"�A� �A��A�bA�JA�1A���A���A���A���A���A���A���A��A���A��A��A��A��HA���A�A��FA���A��A�jA�M�A�1'A�{A�1A�  A��A��`A��
A���A���A�Q�A���A���A���A��uA�E�A��TA�bNA���A� �A��mA��A�1'A���A�"�A��A��\A��DA�z�A�G�A�A��;A�33A�1'A���A�ȴA�VA��+A�|�A�x�A�l�A�ffA�ffA�bNA�XA�9XA��A��A��\A�^5A�{A���A�^5A�?}A��A��wA�O�A��PA�bNA�I�A�33A��A�
=A���A��TA�A��A���A��uA�~�A�p�A�C�A��A�
=A�33A�I�A��yA�A��RA��A���A���A�x�A�  A��!A�S�A�"�A�oA��A�A�n�A�5?A���A�
=A�hsA�1A��!A��HA��A�\)A�I�A�5?A��A���A���A��`A��/A���A���A���A��\A�jA�`BA�O�A�&�A��mA���A�ffA�O�A���A�$�A�ĜA�dZA���A�-A�A��DA�jA�`BA�S�A�I�A�?}A�9XA�+A��A�bA�JA��A�ƨA��9A���A��7A�^5A�K�A�5?A��A���A��9A��A�dZA�C�A��A�JA���A�ȴA�ffA�33A�&�A���A��!A�I�A�A��\A�G�A���A��PA��A��A�  A��+A�(�A��9A�bNA��;A��hA�oA���A��A��DA�jA�ZA�(�A�A��A��A��/A���A���A��A�bNA�JA���A��DA�bNA�+A�E�A�;dA�ȴA�hsA�-A��yA�r�A��-A�{A�ȴA���A��PA�|�A�bNA�A�A��A��A�ƨA��DA���A�G�A�oA��A�E�A�&�A�AƨAXA~�HA~�A}�A}|�A}O�A};dA}�A}A|�A|��A|5?A{�Azv�Az�\Az�DAz��Az��Az��Az��Az�Az��Az1Ax��Ax�Aw�Av�yAvM�Au�FAu"�At�9AtQ�As�AsAs�As+Ar�`Aq%Ao��Ao��Ao�PAo\)AoXAoG�Ao/Ao"�AooAo
=An�/An�DAn�Am�7Al�HAlbAkO�Ajn�Ai�Ai��AiO�Ai�AhȴAh��Ah�DAhv�AhVAh{Ag�#Ag�wAg��Ag�Agt�Ag\)Ag&�Afv�Af=qAf1'Ae�AeC�AedZAeC�AeAd��Ac��Ac��Ac�Acl�Ac`BAcO�Ab�AbffAb{Aa"�A`VA_�mA_�A^�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111133333333333                                            111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B\B�BoBoB:BoBB�B�B4B�B:BoB�B�BuB@B�B�B+B�BB)�BAUBV9B\)BjKB��B�B�[B��B��B�B��B��BB+B$B7�BZ�Bs�B}"B��B��B��B��B`BZ�Bm)BzDB��B�MB�IB��B��B��B�9B�XBn�BX�B33B1BB��B�WB��B�B��B�BUgBGzBK)BT�B'�B�BB	�B
�B
�B
ŢB
��B
��B
tTB
p�B
g8B
HB
�B
eB
 �B	�JB	��B
B
�B
fB	��B	��B	�)B	�#B	��B	�0B	�B	�UB	�@B	��B	�bB	yrB	bB	H�B	B�B	<jB	8�B	#�B	 'B	OB	�B	�B	�B	+B	B	1B�lB�]B� B�dB��BǮB��B�BB�jB�dB��B��B�wB�RB��B�:B��B�VB�IB��B��B��B�_B�SB��B�{B��B�B�iBy�Bu�Bp;ByrBzBrBncB��B��B��B�MB�uB��B��B��B��B�^B��B�B�UBĜB�mBƨB�B��B͟B�B��B��B��B��B�zB�HB��B�;B�2B�`B��B�oB�B�B�ZB�B�|B�|B�BیB��B�#BҽB�?B��B�HB��B��B�wB��B��B��B�wB��B� B��B��B�wB�B��B��BɆB�EB�EB�BȀB��B�RB�B��B�KB��B�5B�WB�#B�B�dB�
B�B� B� B�5B�GB�8B��B�B��B	�B	{B	B	%B	_B	�B		�B	
�B	
�B	B	DB	DB	B	~B	�B	xB	�B	:B	MB	�B	7B	�B	�B	OB	�B	"�B	%B	)*B	*�B	.�B	4nB	<�B	=B	H�B	\]B	a|B	kQB	t�B	u�B	u�B	w�B	x�B	z�B	{�B	{�B	{�B	}�B	��B	��B	�iB	�4B	�4B	��B	�uB	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�:B	�SB	�uB	�:B	�uB	��B	��B	��B	�'B	�-B	�B	��B	�zB	�RB	��B	��B	�*B	�eB	�kB	�B	�B	�!B	��B	��B	��B	��B	�?B	��B	��B	�B	��B	�$B	��B	��B	�jB	�qB	��B	�HB	�B	��B	�'B	��B	�aB	�gB	�tB	�KB	ȴB	ɆB	˒B	��B	��B	�6B	�B	бB	уB	ҽB	�&B	ҽB	ԕB	՛B	�
B	�B	�KB	��B	��B	یB	��B	ܒB	��B	ݘB	��B	��B	�B	�&B	��B	�8B	�DB	��B	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	�B	�B	��B	�+B	��B	�+B	�`B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�xB	�B	�B	��B	��B	��B	��B
 4B
 4B
  B
oB
uB
�B
�B
�B
�B

�B
~B
�B
�B
�B
�B
�B
�B
�B
"B
�B
�B
�B
(B
�B
�B
bB
.B
�B
�B
�B
�B
�B
bB
�B
hB
�B
 B
 B
hB
4B
B
oB
�B
�B
B
�B
B
�B
uB
�B
B
�B
�B
�B
{B
�B
YB
$B
$B
YB
YB
�B
�B
_B
_B
+B
�B
�B
�B
7B
	B
�B
�B
�B
	B
�B
	B
�B
qB
CB
CB
CB
CB
�B
�B
�B
xB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
#:B
#�B
$B
%zB
&LB
&�B
'RB
'RB
'RB
'B
'B
'B
'RB
(�B
)�B
*0B
*�B
+6B
,�B
-�B
.�B
.�B
.�B
/B
/OB
/�B
/�B
/�B
/�B
/�B
0UB
0�B
0�B
0�B
1'B
1'B
1�B
0�B
1[B
1�B
2aB
2aB
33B
3�B
3�B
4�B
5?B
5�B
5�B
6B
7B
8�B
8�B
8�B
9XB
9�B
9�B
:*B
:^B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;dB
;dB
;dB
;dB
;�B
<6B
<�B
=<B
=<B
=<B
=�B
=�B
>BB
>wB
>�B
?B
?HB
?}B
?}B
?�B
?�B
?�B
@B
@OB
@OB
@�B
@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�BB�B�B(B�B�B.B�BhB@BoB4B�BB�B@BhBB@B4B�B�B4B�BBbB�B.B�B�B.B�B�BhBB�B B:BuB�B�BuB�B�B�BoB:B�BB4BoBB�BB�BB�BB�BoB�B�B�B�BuB�B�B�BoBBBoB:BBB�B�B�BFBB@BFBFBB�BB{B�B_B�B$B�B_B$B�B�B�B�BqBB�B�B�B+B	B%zB �B�B�B#:BIBOB'�B+kB'�B,qB*�B,B6B6B3hB8�B9�BA�BL0BMjBS&BR�BS�BV�BV�BXEBVmBY�BX�B[#BY�B\�B]dB[WB]dB_;B]�B_�BaBc�BffBd�BjB�~B��B�:B��B�qB�=B��B��B��B�-B�4B��B�B�LB�XB��B�[B�aB��B�UB��B��B�!B��B��B��B��B�-B��B��B�'B��B�-B��B�tB�nB�-B�nB�LB�B��B�BB�}B�B��B�tB��BҽB�,B��B�[B�yBߤB�TB�,B�sB�B�B��B�B�]B�5B�B��B  BoBoBuB�BuB�B�B�B�B�B�B+B�B1B�B1B_B�B�B,qB1�B5B7�B8�B;dB<6BM�BS�BVmBW�B_Bc�BgBiyBk�Bp;Bx�Bx�Bw�By	B{ByrBzDB~�B}�B�AB��B�{B�B�B��B�+B�lB�~B��B��B�qB�kB�RB��B�B�tB�}B��B�4B��B��B��B�B�3B�B��Bt�BY�BY�B_B\�BffBZ�BU�BHB=<B~�Be`BgBkQBg�B_Bs�Bt�BrB��B�Bv�Bw�ByrBy�BcB��B��B��B�lB��B�PB��B�uB��B��B��B�	B��B��B��B��B�B��B��B�-B�nB��B��B�B�B�OB�hB�^B��B�XB�jB�B̘B�XB��BȴBȀBȴBɺBӏB�/B�qB��B�RB�B��B��B�aB�B�1B��B��B��B�MBv`BjBYBW�BZ�BS�BL�Bu%Bc�BD3BJ�B�uB/OBB�B�B�B�B�B�BCB�B�B�BDB�BVB�xB��B��B iB��B��B�B��B��B�B��B�)B�)B��B��B�KB�B�yB��B�WB�B��B�B�B�0B�gB��B�<B�jB�XBɺBB��B��B�RB�xB��B�RB��B��B��BtBv+B�MB~�B[�BK�BW
BM6BH�BE�BPHBEmBF�BGzBGEBD�BH�BFBL0BEmBF�BI�BO�BZB�BT�BN�BM6BGzB=BAUB6zB-�B�BB_B�BB$BFB�B�B@B�BBB�B4B�BJBB
	B�B�B�B(B
�(BB
�DB
��B
��B
��B
��B
�B
�B
�B
�iB
�ZB
�#B
�,B
�EB
�B
��B
�FB
��B
�[B
�B
�B
��B
��B
�tB
��B
��B
cB
|PB
|�B
yrB
x8B
�4B
tTB
r|B
o�B
oiB
m�B
m]B
sB
u�B
qvB
lWB
o�B
d�B
f2B
�\B
n/B
Z�B
N�B
OBB
?B
J�B
T�B
Z�B
'�B
�B
�B
�B
~B
B
B
�B
SB
�B
.B
�B
�B
�B
 �B	�]B	��B	�PB
AB	��B
B	��B
oB	��B	��B	�B	�|B	�B	��B	��B	��B	��B	��B
  B
B
1B
�B
�B
1B
	lB
�B
B
	�B
�B
\B
_B
uB
;B	��B	�xB	�B	��B	�B	�B	��B
1�B	�B	�|B	��B	�BB	�)B	�)B	��B	��B	�B	خB	ٴB	ޞB	�B	�B	�jB	�9B	�mB	��B	ȀB	�OB	�OB	�qB	�}B	��B	��B	�LB	��B	�0B	�B	�aB	��B	��B	�UB	��B	�-B	B	�B	��B	��B	��B	�xB	��B	�$B	�qB	�9B	�1B	��B	�oB	�\B	�"B	�$B	��B	�	B	�nB	��B	|�B	�;B	v�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                            444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B�B�B�B�B�B�BVB!B�B�B!B�B�B�B'B�B�B�B.B{B�BkB&LB=�BR�BXyBf�B�0B�XB��B�B�'B�[B�B�JB�\B{BtB49BW>BpByrB��B�B��B�CB\]BV�BiyBv�B�GB��B��B�B��B�?B��B��Bj�BT�B/�B�BoB��B�B�"B�aB�OB�kBQ�BC�BGyBP�B$BBVB%B
�fB
�B
��B
�OB
}"B
p�B
m(B
c�B
DgB
�B
�B	��B	��B	��B	�VB
	�B
�B	�AB	�(B	�yB	�sB	�#B	��B	�[B	��B	��B	�B	��B	u�B	^iB	D�B	?HB	8�B	5B	�B	wB	�B	�B	�B	B	{B	hB	�B��BحB�pBȴB�B��B�&B��B��B��B��B�EB��B��B�-B��B�OB��B��B��B�LB�*B��B��B��B��B�'B�_B|�Bu�BrGBl�Bu�Bv`BncBj�B� B�7B~�B��B��B�B��B� B��B��B�B�^B��B��B��B��B�gB�)B��B�gB� B�B��B�B��BݘB�,B�B�B�B��B�B�B�fB�B�B��B��B�QB��B�>B�sB�BB�HB��B��B�6B��B�B�B�B��B�B�pB�<B�B��B�XB�9B�B��BÕBÕB�gB��B�3BŢB�TB�8B՛B�8BڅBקB�sB�mBٴB�ZB��B�PB�PB�B�B�B�>B�lB�B�(B��B	 iB	uB	�B	MB	%B	�B	+B	_B	�B	�B	eB	�B	
=B	�B	IB	�B	�B	B	�B	�B	B	�B	CB	!B	!bB	%zB	'B	+6B	0�B	8�B	9XB	D�B	X�B	]�B	g�B	p�B	rGB	rB	s�B	t�B	w1B	xB	xB	x7B	y�B	}"B	|�B	|�B	|�B	|�B	}"B	~�B	�oB	��B	~�B	}�B	}VB	�GB	�+B	�%B	��B	�@B	� B	�B	�+B	�B	�B	�=B	��B	��B	��B	��B	��B	�.B	�4B	��B	�wB	�}B	�UB	��B	��B	��B	��B	��B	�zB	��B	��B	�dB	�kB	�qB	��B	�BB	��B	�'B	��B	�-B	��B	�mB	��B	�tB	�B	��B	��B	��B	�)B	��B	�jB	�<B	�wB	�B	��B	��B	��B	ěB	�B	��B	��B	�B	�B	ɆB	�dB	�B	��B	�B	�vB	�B	��B	��B	�ZB	�`B	՛B	�>B	�>B	��B	�B	��B	�B	��B	�)B	�5B	�B	�vB	�GB	�B	�B	�8B	��B	��B	��B	�>B	�DB	�JB	�B	�WB	�cB	�5B	�oB	��B	�AB	�{B	�B	�{B	�B	��B	��B	�B	��B	�+B	�+B	�+B	��B	�`B	�+B	�fB	��B	��B	�lB	�>B	�JB	��B	��B	��B	��B	�PB	��B	��B	�.B
@B
GB
MB
+B
�B
	7B
	�B
	�B

=B

	B

=B

=B

rB

	B
CB

	B
xB
IB
�B
�B
~B
B
B
IB
�B
�B
�B
!B
�B
B
PB
PB
�B
�B
VB
�B
�B
'B
\B
'B
\B
�B
�B
.B
\B
'B
�B
.B
�B
�B
�B
tB
tB
�B
�B
B
FB
�B
�B
{B
�B
LB
B
�B
YB
$B
$B
$B
YB
$B
YB
�B
�B
�B
�B
�B
�B
0B
0B
0B
�B
7B
�B
B
=B
=B
B
B
�B
IB
�B
�B
 'B
 [B
!�B
"�B
#B
#�B
#�B
#�B
#nB
#nB
#nB
#�B
%FB
&LB
&�B
'B
'�B
(�B
*0B
+B
+B
+6B
+kB
+�B
+�B
+�B
+�B
,B
,<B
,�B
-BB
-BB
-BB
-wB
-wB
-�B
-BB
-�B
.B
.�B
.�B
/�B
/�B
0 B
0�B
1�B
1�B
2-B
2aB
3gB
5B
5?B
5?B
5�B
6B
6B
6zB
6�B
7KB
7B
7B
7KB
7KB
7KB
7KB
7�B
7�B
7�B
7�B
7�B
8�B
9#B
9�B
9�B
9�B
:)B
:)B
:�B
:�B
;0B
;dB
;�B
;�B
;�B
<B
<6B
<6B
<jB
<�B
<�B
<�B
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B_BB�BxBIB
	B~BB�B�B�B�B.BVB�B�B�B\B�B�B'B'B�B�BVB�B'B~BB�B~BIB.B�BVB'BPB�B�B�B�B�B!B!B�B�B�B.B\B�B�B\B!B\B�BVB'BbB�B�B4B�B�B�B�B'B.B.B�BVBbB�B�BbB\B�B4B�B�BbB�B�B�BVB'BbB�B�B�B�BtBB�BtBBBLBLB�B_BBB�B{BYB!�B�BCBB�B�B�B#�B'�B$@B(�B'B(XB2aB2aB/�B5B6EB=�BH�BI�BOvBOBBPHBS&BS&BT�BR�BV8BU2BWsBVBX�BY�BW�BY�B[�BZB\)B]cB`Bb�B`�Bf�B��B��B��B��B��B��B�B��B�B�}B��B��B�[B��B��B��B��B��B�B��B��B�IB�qB��B�B��B�BB�}B�B��B�wB�B�}B�B��B��B�}B��B��B�RB��B��B��B�^B�BB��B�/B�B�|B�BϫB��B��BߤB�|B��B�lB��B�2B�
B�B�B�]B�%B�PB��B��B��B  B��BB�BGBBGB�B{BMB�B
	B�B�B�BB(�B.IB1[B3�B5?B7�B8�BJ#BPBR�BT,B[WB_�BcTBe�Bh
Bl�Bt�Bu%BtBuYBwfBu�Bv�Bz�BzDB~�B.B�B{�B~\B��B�{B��B��B�CB�B��B��B��B��B�nB��B��B�9B��B��B�3B��B�kB��B�_B�BqABV8BVB[WBYKBb�BW>BR BDgB9�B{Ba�BcTBg�Bc�B[WBp;Bp�BncB.B|BsMBtBu�Bv+B{�B~(B~�B�:B��B�CB��B��B��B��B�B��B�YB��B��B��B��B�kB��B�CB�}B��B��B��B�RB�dB��B��B��B�BƨBɺB�jB��BƨB�?B�B��B�B�
B��B�B��B��B��B�QB��B�0B��B�kB��B�B�B�+B��Br�Bf�BU�BT,BV�BPBH�BquB`AB@�BGB~�B+�BeB*B0B�BLB�BB�B�BB@B�B�B
�B��B�MB�B��B��B�B��B�DB�>B�lB�>B�yB�yB�B�B�B��B��B�B�B� B�JB� B��BȀB��B��B��B��B��B�
B��B��B��B��B��B�CBŢB�B�GB.BpoBr{B��B{BXBG�BSZBI�BEBA�BL�BA�BC,BC�BC�B@�BE9BB[BH�BA�BC,BF
BL/BVmB�kBP�BK)BI�BC�B9XB=�B2�B)�BB_B�B�BnBtB�BFB'B�B�BhBhB�B�B	�B�B_BYBBBBxB
�xBoB
��B
�.B
�GB
��B
��B
�
B
�fB
��B
�B
�B
�sB
�|B
ԕB
�[B
�9B
��B
�B
��B
�UB
�UB
�*B
�IB
��B
��B
�!B
{�B
x�B
x�B
u�B
t�B
|�B
p�B
n�B
l"B
k�B
jB
i�B
oiB
rGB
m�B
h�B
k�B
aB
b�B
��B
jB
W>B
J�B
K�B
;dB
GB
QB
W
B
#�B
CB
CB
*B
�B
RB
RB
0B
�B
FB
*dB
�B

=B
�B	��B	��B	�B	��B	��B	�JB	�bB	�B	��B	�AB	��B	��B	��B	�]B	�B	�B	�B	�AB	�%B	�PB
 iB
�B
MB
@B
�B
�B
B
eB
%B
1B
�B
�B	��B	��B	�B	��B	�oB	�.B	� B	��B	�JB
.IB	��B	��B	�)B	ܒB	�yB	�yB	�#B	�EB	�gB	��B	�B	��B	�`B	�gB	ںB	҉B	ҽB	�B	��B	��B	��B	��B	��B	�B	�B	��B	�3B	��B	�[B	��B	�B	��B	��B	�B	�}B	��B	�nB	�9B	��B	�CB	��B	��B	�tB	��B	��B	��B	��B	��B	��B	�rB	�tB	�CB	�YB	��B	��B	y	B	}�B	r�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                            444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721224957                            20230721224957AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122495720230721224957  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122495720230721224957QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122495720230721224957QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8800            800             