CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:50:13Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  Tp   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Z�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  s$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  yH   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  `   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � %8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` =�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   >(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   D(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   J(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T P(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   P|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   P�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   P�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   P�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � P�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   Q   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   Q8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    Q@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        Q`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        Qh   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       Qp   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    QxArgo profile    3.1 1.2 19500101000000  20230721225013  20230721225013  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�:�8!k`@�:�8!k`11  @�:�l��@�:�l��@1�I<��
@1�I<��
�d�������d������11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 BA  BA  FF  ?�  @   @=p�@z�H@�  @�G�@޸RA   AG�A ��A,(�A@��A_\)A~�RA�\)A��A�  A�\)AϮA�  A�  B   B  B(�BQ�B Q�B(Q�B0  B7�B@(�BH(�BO�
BW�
B`  Bg�
Bp  Bx(�B�{B�{B�  B�  B�{B�  B�{B�{B�{B�{B�{B�{B��
B�  B�  B�  B�  B�  B�  B�  B�{B�{B�  B��B�{B�(�B�  B��
B��B�  B�{B�{C   C  C  C��C{C

=C��C�C��C  C
=C  C�C��C�C
=C 
=C!��C$  C&
=C(
=C*  C+��C.  C0  C2  C4  C6
=C8  C:  C<  C>  C@  CA��CD
=CF{CH{CJ
=CL  CN  CO��CR  CT{CV
=CW�CZ  C\  C]��C_�Ca��Cd  Cf
=Ch  Cj
=Cl{Cn
=Cp  Cr
=Ct
=Cv{Cx{Cz  C|  C~
=C��C���C�  C�C�C�C�  C�  C�C�C�
=C�C���C�  C�C���C�  C�  C���C�  C�  C���C���C�C�
=C�C�  C�  C�C�C�  C���C�C���C�  C��C�  C���C���C���C�  C�\C�  C���C���C���C�C�
=C�C���C���C�C�  C�C�C�
=C�C�
=C���C���C���C���C���C���C���C�  C�C�
=C�  C���C��C���C�C�  C�  C�  C���C���C���C���C���C��C���C�  C���C���C�C�
=C�\C�
=C�  C���C���C�  C���C���C���C���C���C�  C���C���C���C�C�
=C�  C���C���C���C���C�  C���C�  C�
=C�C�  C�C���C��C���C�  C�  C�  C���C�C�\C�\C�\D D � D  D�D�D}qD�qD� D  D}qD  D��D�D��D�D��D�D��D	D	�D
D
��D�D��D  D}qD  D� D  D�DD�D�D� D�D� D��D}qD�D��DD�D�D��D�D� D�qD� D  D� DD��D�D��D  D}qD  D}qD��D��D  D��DD� D�qD ��D!  D!}qD!�qD"� D"�qD#� D$  D$}qD%�D%��D&�D&��D'�D'}qD(  D(��D)  D)� D*  D*� D+�D+��D,  D,� D,�qD-� D.  D.��D/�D/}qD0  D0��D1  D1��D2D2��D3  D3��D3�qD4� D5  D5}qD5��D6}qD7  D7� D8  D8}qD8�qD9}qD9�qD:� D;  D;��D<  D<��D=�D=� D>  D>��D?  D?��D@�D@� DA�DA��DB  DB� DCDC�DD�DD��DE  DE� DF  DF}qDG  DG}qDH  DH��DI�DI� DJ  DJ� DK�DK��DL�DL��DM�DM��DN  DN��DODO�DPDP� DP�qDQz�DR  DR� DS  DS� DS�qDT}qDT��DU� DV  DV� DW�DW}qDW�qDX� DY�DY}qDZ  DZ��D[�D[� D\�D\��D]�D]�D^�D^z�D^��D_� D`  D`� Da  Da��Db�Db}qDc�Dc��Dc��Ddz�Dd�qDe}qDe�qDf� Dg  Dg� DhDh� Dh�qDi��DjDj� Dk  Dk��Dk��DlxRDl�qDm� Dn  Dn� Dn�qDoz�Do��Dp}qDq  Dq� Dr�Dr� Dr��Ds� Dt�Dt}qDt�qDu� Dv�Dv�Dw�Dw� Dx  Dx��Dy�Dy}qDy�qDz}qDz�qD{}qD{�qD|}qD|��D}}qD~�D~� D  D��D�HD�>�D�~�D���D���D�@ D��HD��HD�HD�AHD��HD��HD�HD�@ D�~�D�� D���D�>�D�~�D���D�  D�AHD��HD��HD��D�AHD��HD��HD�HD�AHD�� D���D���D�=qD�� D�� D�  D�@ D��HD���D��D�AHD�� D�� D�  D�AHD�� D�� D�  D�@ D��HD��HD�HD�AHD�� D���D�  D�@ D�� D���D���D�@ D�� D��HD�  D�@ D��HD��HD�HD�AHD�� D���D�  D�AHD��HD��HD�  D�@ D�� D��HD�HD�AHD��HD�� D�  D�@ D�� D�� D���D�>�D�� D�� D�  D�AHD�� D���D�  D�@ D��D��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?\)?#�
?L��?��?�\)?�{?���?�(�@   @��@
=@.{@=p�@G�@aG�@h��@}p�@��
@�{@�@��H@��@��@��@�(�@�G�@˅@��@�
=@�G�@�ff@���@�@���A33AA	��A{A��AffA�A�A   A"�\A'
=A+�A-p�A1G�A5A8Q�A<(�A@  AB�\AFffAH��AMp�AP��AS33AW�AY��A_\)AaG�AeAj=qAl(�Ap��Au�Aw
=A|��A\)A���A��
A�p�A�  A���A�33A�{A�
=A���A��A�z�A�
=A���A�=qA�(�A�A�  A�G�A��HA�p�A�ffA���A��HA��
A�{A�  A�G�A�(�A��A�
=A���A��\A��A�
=A�Q�A�33A�z�A�\)A���A�33A�p�AθRAљ�A�33A���A�  A���A��
A�A߮A�\A��
A�RA�Q�A�\A��A�ffA�A�33A��A�  A�G�A�(�A�B   BG�B{B\)B��BG�B�\B�
B��B
=qB
=Bz�Bp�BffB(�B��BffB33B��Bp�B�RB�
B��B�\B\)B��BB�HB (�B ��B"�\B#
=B$��B%��B&�RB((�B(��B*ffB+\)B,z�B-B.�\B0  B1�B1�B3�B4Q�B5G�B6�HB7�B8��B:=qB:�HB<��B=G�B>�\B?�
B@��BB�\BC\)BD��BEBG
=BH(�BIG�BJ�RBK�BM�BN=qBO
=BP��BQp�BR�HBT(�BT��BV�\BW�BX��BZ=qB[
=B\��B]p�B^�HB`(�B`��Bb�\Bc�
Bd��Bf{Bg�BhQ�Bi�Bk
=Bl  BmBn�\Bp  BqG�Br=qBs�
Bt��Bu�Bw�Bx(�By��B{
=B{�
B}p�B~�\B�B��\B�
=B�p�B�(�B��RB�33B��B�Q�B��HB���B�  B��\B�G�B���B�(�B��RB��B�B�ffB���B�33B��
B�(�B��RB�p�B��B�Q�B��HB�33B�B�ffB���B�G�B��
B�=qB��RB�\)B�B�{B���B�33B���B�=qB���B���B��B�{B�Q�B��HB�p�B��B�=qB���B�
=B���B�{B�ffB�
=B�p�B��
B�z�B���B�\)B��B�(�B��HB�33B���B�=qB��\B�
=B��B��B�ffB���B�p�B��B�Q�B���B�
=B�p�B�{B��\B���B�\)B��B�Q�B���B�33B���B��
B�ffB���B��B���B�  B�Q�B��\B��B�\)B���B�{B��\B��HB�33B�B�(�B�z�B���B��B�B�{B��\B��B�\)B��B�  B�z�B��HB��B��B�  B�ffB���B��HB�G�B�B�{B�Q�B��RB�33B�p�B��B�=qB��\B���B��B��B�  B�Q�B��RB�G�B�p�B�{B�z�B��RB�\)B��
B�(�B£�B�33BÙ�B��B�z�B���B�G�B��B�ffBƸRB�33B�B�=qBȏ\B��B�B�(�B�z�B�
=BˮB�  B�ffB�
=BͅB��
B�ffB�
=B�p�B��
BЏ\B�
=Bљ�B��B�ffB�
=BӅB��
B�ffB�
=Bՙ�B��B�z�B��B�p�B�  Bأ�B�
=BمB�{Bڣ�B�
=B�p�B�{Bܣ�B���B�p�B�{B�ffB��HB߅B��B�Q�B��HB�p�B�B�=qB���B�G�B�B�=qB��HB�G�B�B�Q�B��HB�33B�B�Q�B���B�33B�B�ffB���B�33B�B�Q�B��B��B�B�Q�B��B�
=B�B�=qB��\B�
=B�B�{B�z�B��HB�B��B�Q�B���B��B��B�Q�B���B���B�  B�ffB���B���B��B�z�B��B��B��
B�z�B��B�p�B��B��\B�
=B�p�B��C G�C z�C �RC  CG�Cp�C��C
=C33C�C��C  C33C�C��C  C(�Cz�CC  C(�CffC�RC�C{C\)C�C�HC
=CQ�C��C�
C{CG�C��C�
C	  C	=qC	��C	C	��C
G�C
�\C
C  C\)C�\CC�CffC�\C�C33CffC��C  C(�CffCC  C33C�CC��CQ�C�\CC
=C\)C�\C�
C(�CffC��C�C=qCffC�C  C(�Cp�C��C  C=qC��C�RC
=C\)C�\C��C(�CffC��C��C33Cp�CC�CQ�C�\C�HC=qCp�C�C  CG�Cz�C�
C�CQ�C�\C�HC33Cz�C�C�CG�C�C�RC  C\)C�\CC �C ffC ��C �
C!33C!p�C!��C!��C"G�C"�\C"�RC#
=C#\)C#��C#�
C${C$p�C$�RC$��C%=qC%�\C%C&
=C&\)C&��C&�
C'�C'z�C'C'��C(=qC(�\C(��C)
=C)ffC)�C)�C*(�C*z�C*��C+
=C+=qC+�C+�
C,(�C,p�C,��C,�HC-=qC-z�C-�C.  C.Q�C.�C.C/
=C/ffC/�\C/�
C033C0p�C0��C0��C1G�C1z�C1�RC2�C2\)C2�\C2�
C333C3z�C3�C3��C4Q�C4��C4��C5{C5ffC5�RC5��C6=qC6��C6�HC7
=C7Q�C7�C8  C8G�C8z�C8��C9(�C9\)C9��C9��C:G�C:�C:�RC;
=C;\)C;�C;�C<(�C<p�C<�
C=
=C=G�C=��C=��C>=qC>p�C>��C?�C?\)C?�\C?�HC@=qC@�\C@��CA  CAG�CA��CA��CB(�CBffCB�RCC{CC\)CC��CC�
CD(�CD�CDCE  CE=qCE��CE��CF33CFp�CF�CG  CGQ�CG��CG�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111133333333333                                                                                                                                                                         111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  @   @=p�@z�H@�  @�G�@޸RA   AG�A ��A,(�A@��A_\)A~�RA�\)A��A�  A�\)AϮA�  A�  B   B  B(�BQ�B Q�B(Q�B0  B7�B@(�BH(�BO�
BW�
B`  Bg�
Bp  Bx(�B�{B�{B�  B�  B�{B�  B�{B�{B�{B�{B�{B�{B��
B�  B�  B�  B�  B�  B�  B�  B�{B�{B�  B��B�{B�(�B�  B��
B��B�  B�{B�{C   C  C  C��C{C

=C��C�C��C  C
=C  C�C��C�C
=C 
=C!��C$  C&
=C(
=C*  C+��C.  C0  C2  C4  C6
=C8  C:  C<  C>  C@  CA��CD
=CF{CH{CJ
=CL  CN  CO��CR  CT{CV
=CW�CZ  C\  C]��C_�Ca��Cd  Cf
=Ch  Cj
=Cl{Cn
=Cp  Cr
=Ct
=Cv{Cx{Cz  C|  C~
=C��C���C�  C�C�C�C�  C�  C�C�C�
=C�C���C�  C�C���C�  C�  C���C�  C�  C���C���C�C�
=C�C�  C�  C�C�C�  C���C�C���C�  C��C�  C���C���C���C�  C�\C�  C���C���C���C�C�
=C�C���C���C�C�  C�C�C�
=C�C�
=C���C���C���C���C���C���C���C�  C�C�
=C�  C���C��C���C�C�  C�  C�  C���C���C���C���C���C��C���C�  C���C���C�C�
=C�\C�
=C�  C���C���C�  C���C���C���C���C���C�  C���C���C���C�C�
=C�  C���C���C���C���C�  C���C�  C�
=C�C�  C�C���C��C���C�  C�  C�  C���C�C�\C�\C�\D D � D  D�D�D}qD�qD� D  D}qD  D��D�D��D�D��D�D��D	D	�D
D
��D�D��D  D}qD  D� D  D�DD�D�D� D�D� D��D}qD�D��DD�D�D��D�D� D�qD� D  D� DD��D�D��D  D}qD  D}qD��D��D  D��DD� D�qD ��D!  D!}qD!�qD"� D"�qD#� D$  D$}qD%�D%��D&�D&��D'�D'}qD(  D(��D)  D)� D*  D*� D+�D+��D,  D,� D,�qD-� D.  D.��D/�D/}qD0  D0��D1  D1��D2D2��D3  D3��D3�qD4� D5  D5}qD5��D6}qD7  D7� D8  D8}qD8�qD9}qD9�qD:� D;  D;��D<  D<��D=�D=� D>  D>��D?  D?��D@�D@� DA�DA��DB  DB� DCDC�DD�DD��DE  DE� DF  DF}qDG  DG}qDH  DH��DI�DI� DJ  DJ� DK�DK��DL�DL��DM�DM��DN  DN��DODO�DPDP� DP�qDQz�DR  DR� DS  DS� DS�qDT}qDT��DU� DV  DV� DW�DW}qDW�qDX� DY�DY}qDZ  DZ��D[�D[� D\�D\��D]�D]�D^�D^z�D^��D_� D`  D`� Da  Da��Db�Db}qDc�Dc��Dc��Ddz�Dd�qDe}qDe�qDf� Dg  Dg� DhDh� Dh�qDi��DjDj� Dk  Dk��Dk��DlxRDl�qDm� Dn  Dn� Dn�qDoz�Do��Dp}qDq  Dq� Dr�Dr� Dr��Ds� Dt�Dt}qDt�qDu� Dv�Dv�Dw�Dw� Dx  Dx��Dy�Dy}qDy�qDz}qDz�qD{}qD{�qD|}qD|��D}}qD~�D~� D  D��D�HD�>�D�~�D���D���D�@ D��HD��HD�HD�AHD��HD��HD�HD�@ D�~�D�� D���D�>�D�~�D���D�  D�AHD��HD��HD��D�AHD��HD��HD�HD�AHD�� D���D���D�=qD�� D�� D�  D�@ D��HD���D��D�AHD�� D�� D�  D�AHD�� D�� D�  D�@ D��HD��HD�HD�AHD�� D���D�  D�@ D�� D���D���D�@ D�� D��HD�  D�@ D��HD��HD�HD�AHD�� D���D�  D�AHD��HD��HD�  D�@ D�� D��HD�HD�AHD��HD�� D�  D�@ D�� D�� D���D�>�D�� D�� D�  D�AHD�� D���D�  D�@ D��D��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?\)?#�
?L��?��?�\)?�{?���?�(�@   @��@
=@.{@=p�@G�@aG�@h��@}p�@��
@�{@�@��H@��@��@��@�(�@�G�@˅@��@�
=@�G�@�ff@���@�@���A33AA	��A{A��AffA�A�A   A"�\A'
=A+�A-p�A1G�A5A8Q�A<(�A@  AB�\AFffAH��AMp�AP��AS33AW�AY��A_\)AaG�AeAj=qAl(�Ap��Au�Aw
=A|��A\)A���A��
A�p�A�  A���A�33A�{A�
=A���A��A�z�A�
=A���A�=qA�(�A�A�  A�G�A��HA�p�A�ffA���A��HA��
A�{A�  A�G�A�(�A��A�
=A���A��\A��A�
=A�Q�A�33A�z�A�\)A���A�33A�p�AθRAљ�A�33A���A�  A���A��
A�A߮A�\A��
A�RA�Q�A�\A��A�ffA�A�33A��A�  A�G�A�(�A�B   BG�B{B\)B��BG�B�\B�
B��B
=qB
=Bz�Bp�BffB(�B��BffB33B��Bp�B�RB�
B��B�\B\)B��BB�HB (�B ��B"�\B#
=B$��B%��B&�RB((�B(��B*ffB+\)B,z�B-B.�\B0  B1�B1�B3�B4Q�B5G�B6�HB7�B8��B:=qB:�HB<��B=G�B>�\B?�
B@��BB�\BC\)BD��BEBG
=BH(�BIG�BJ�RBK�BM�BN=qBO
=BP��BQp�BR�HBT(�BT��BV�\BW�BX��BZ=qB[
=B\��B]p�B^�HB`(�B`��Bb�\Bc�
Bd��Bf{Bg�BhQ�Bi�Bk
=Bl  BmBn�\Bp  BqG�Br=qBs�
Bt��Bu�Bw�Bx(�By��B{
=B{�
B}p�B~�\B�B��\B�
=B�p�B�(�B��RB�33B��B�Q�B��HB���B�  B��\B�G�B���B�(�B��RB��B�B�ffB���B�33B��
B�(�B��RB�p�B��B�Q�B��HB�33B�B�ffB���B�G�B��
B�=qB��RB�\)B�B�{B���B�33B���B�=qB���B���B��B�{B�Q�B��HB�p�B��B�=qB���B�
=B���B�{B�ffB�
=B�p�B��
B�z�B���B�\)B��B�(�B��HB�33B���B�=qB��\B�
=B��B��B�ffB���B�p�B��B�Q�B���B�
=B�p�B�{B��\B���B�\)B��B�Q�B���B�33B���B��
B�ffB���B��B���B�  B�Q�B��\B��B�\)B���B�{B��\B��HB�33B�B�(�B�z�B���B��B�B�{B��\B��B�\)B��B�  B�z�B��HB��B��B�  B�ffB���B��HB�G�B�B�{B�Q�B��RB�33B�p�B��B�=qB��\B���B��B��B�  B�Q�B��RB�G�B�p�B�{B�z�B��RB�\)B��
B�(�B£�B�33BÙ�B��B�z�B���B�G�B��B�ffBƸRB�33B�B�=qBȏ\B��B�B�(�B�z�B�
=BˮB�  B�ffB�
=BͅB��
B�ffB�
=B�p�B��
BЏ\B�
=Bљ�B��B�ffB�
=BӅB��
B�ffB�
=Bՙ�B��B�z�B��B�p�B�  Bأ�B�
=BمB�{Bڣ�B�
=B�p�B�{Bܣ�B���B�p�B�{B�ffB��HB߅B��B�Q�B��HB�p�B�B�=qB���B�G�B�B�=qB��HB�G�B�B�Q�B��HB�33B�B�Q�B���B�33B�B�ffB���B�33B�B�Q�B��B��B�B�Q�B��B�
=B�B�=qB��\B�
=B�B�{B�z�B��HB�B��B�Q�B���B��B��B�Q�B���B���B�  B�ffB���B���B��B�z�B��B��B��
B�z�B��B�p�B��B��\B�
=B�p�B��C G�C z�C �RC  CG�Cp�C��C
=C33C�C��C  C33C�C��C  C(�Cz�CC  C(�CffC�RC�C{C\)C�C�HC
=CQ�C��C�
C{CG�C��C�
C	  C	=qC	��C	C	��C
G�C
�\C
C  C\)C�\CC�CffC�\C�C33CffC��C  C(�CffCC  C33C�CC��CQ�C�\CC
=C\)C�\C�
C(�CffC��C�C=qCffC�C  C(�Cp�C��C  C=qC��C�RC
=C\)C�\C��C(�CffC��C��C33Cp�CC�CQ�C�\C�HC=qCp�C�C  CG�Cz�C�
C�CQ�C�\C�HC33Cz�C�C�CG�C�C�RC  C\)C�\CC �C ffC ��C �
C!33C!p�C!��C!��C"G�C"�\C"�RC#
=C#\)C#��C#�
C${C$p�C$�RC$��C%=qC%�\C%C&
=C&\)C&��C&�
C'�C'z�C'C'��C(=qC(�\C(��C)
=C)ffC)�C)�C*(�C*z�C*��C+
=C+=qC+�C+�
C,(�C,p�C,��C,�HC-=qC-z�C-�C.  C.Q�C.�C.C/
=C/ffC/�\C/�
C033C0p�C0��C0��C1G�C1z�C1�RC2�C2\)C2�\C2�
C333C3z�C3�C3��C4Q�C4��C4��C5{C5ffC5�RC5��C6=qC6��C6�HC7
=C7Q�C7�C8  C8G�C8z�C8��C9(�C9\)C9��C9��C:G�C:�C:�RC;
=C;\)C;�C;�C<(�C<p�C<�
C=
=C=G�C=��C=��C>=qC>p�C>��C?�C?\)C?�\C?�HC@=qC@�\C@��CA  CAG�CA��CA��CB(�CBffCB�RCC{CC\)CC��CC�
CD(�CD�CDCE  CE=qCE��CE��CF33CFp�CF�CG  CGQ�CG��CG�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111133333333333                                                                                                                                                                         111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�XA�bNA�hsA�l�A�t�A�n�A�ffA�r�A�z�A�z�A�|�A�|�A�|�A�~�A�~�A�~�A�|�A�|�A�|�A�~�AځAځAڃAځAځAڅAڅAڅAڅAڇ+Aڇ+Aڇ+Aڇ+AڋDAڏ\AڍPAڏ\AړuAڑhAڏ\Aڏ\AڑhAڑhAڑhAډ7A�t�A�z�A�t�A�t�A�t�A�K�A�5?A�1'Aա�A�A�bA�\)A�p�A�33A�oA�r�A�ȴA�ƨA��A���AƬA�jA�|�Aç�A+A��wA���A��uA��#A�VA��
A���A�  A���A��-A���A�=qA��A�bNA��A�z�A��jA�A� �A�oA�Q�A�bNA�ZA�{A���A�bNA��DA�I�A�r�A�1A�A��
A�v�A��A�=qA��/A���A�ffA��HA�  A��#A�Q�A�9XA��FA�x�A�C�A��^A��A~bA{�Aw�Aq��Ao��AlQ�AfQ�Aax�A_C�A^  A\  AW�mAUt�AR�AP�yAN�DAJI�AG�mAF�yAF �AE�ACXAAdZA@E�A>JA<�!A;�#A;VA:ffA9?}A8��A7�wA5ƨA3;dA2^5A0�jA/�FA/33A.��A-�wA,��A,ZA+7LA*�A)&�A'�;A&A�A$~�A!�AC�A �A
=AhsAS�A1AG�A&�A��AbNAA�jA�A$�A�#AS�A��AI�AVAȴAĜA��AjA��A
jA
=qA
JA
A	��A	l�AbNA��A �A�Ar�Av�AE�A�@�t�@�;d@���@�"�@��@���@�E�@��@���@�"�@�@�=q@�X@�O�@��#@�{@���@���@���@�%@�9X@�%@�n�@�  @�^5@��T@�@�ȴ@���@���@�j@�F@��;@�C�@�33@�|�@�+@�@߾w@�@���@ܼj@�A�@ۥ�@�C�@�n�@ڸR@�`B@��@�G�@��@�J@ٺ^@�I�@�z�@��@��m@ו�@�33@ְ!@Ցh@���@�  @�l�@�n�@�{@�X@ЋD@υ@�o@�@���@�{@�X@��`@˝�@���@ɡ�@�A�@�C�@�V@��
@���@�@�v�@�M�@�{@��#@���@�~�@�  @�K�@���@�9X@��F@���@�`B@�~�@�V@�`B@��@��@���@��u@�  @��@�;d@��+@��@���@�x�@���@���@��@��/@�Z@�1'@�bN@��P@�n�@�X@�G�@���@�p�@�7L@��@��`@�O�@�`B@�G�@��`@���@�b@��;@��w@�\)@�ȴ@��y@�o@���@�J@�G�@��j@�(�@�b@��@�ƨ@��
@�dZ@�+@��H@�v�@�=q@�@��T@�@��h@�`B@�X@�O�@��@��@�bN@�  @��w@�t�@�@�v�@��@��7@�/@�z�@��;@���@��@���@��P@�t�@�33@�
=@��@��\@�n�@�=q@��@�@��-@�/@��@��`@��9@�r�@��@��
@���@��w@��P@�33@���@���@�$�@��h@��@��j@��@�9X@�1@��@��@��\@�v�@�5?@��@��`@��j@���@�bN@�1'@��m@�t�@�S�@�"�@��R@�~�@�J@���@�G�@��@�bN@��
@��@��@��@��@��+@��@��@���@�@�x�@�%@��u@�bN@�bN@�bN@�Q�@�(�@���@�|�@�33@�o@��@���@�v�@�5?@�J@�@��`@��@�(�@�1@�  @��;@��w@��P@�K�@�@��\@�ff@�5?@�{@���@�hs@�G�@��@��/@��j@���@��@�I�@��;@�dZ@�+@�@�ff@�=q@�J@��T@�@��7@�hs@�G�@�7L@�/@�/@�7L@�/@�&�@��@���@��/@��u@�1'@��
@��F@�l�@�33@��@�n�@�-@���@�X@�%@���@�j@�1'@���@�ƨ@��F@��P@�o@��!@�~�@�^5@�5?@���@��#@��^@�x�@�7L@�V@��`@��j@��@��D@�Z@��@�w@�@��@|�@;d@~�@~��@~ff@}�@}?}@|�@|9X@{S�@z��@zJ@y%@x�9@x��@x�u@x�@xb@w�w@w|�@wK�@v��@v�@vff@v$�@v{@u�-@u`B@tZ@s�@rn�@qX@p�u@p  @o�w@r��@q��@q7L@pbN@o��@n�@n�+@m�T@m�@mO�@l�@l��@lI�@l1@kƨ@k33@j�\@j-@i�^@iX@h��@h��@hQ�@hb@g�@gl�@f��@f��@f5?@e@e?}@d��@dZ@d�@c�m@ct�@cS�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�XA�VA�ZA�K�A�XA�bNA�dZA�^5A�dZA�hsA�dZA�jA�hsA�ffA�ffA�bNA�l�A�n�A�v�A�v�A�t�A�v�A�t�A�p�A�r�A�l�A�n�A�hsA�dZA�hsA�ffA�hsA�n�A�r�A�v�A�r�A�z�A�~�A�x�A�~�A�x�A�|�A�|�A�x�A�z�A�~�A�z�A�|�A�~�A�z�A�z�A�~�A�z�A�~�A�z�A�~�A�|�A�z�AځA�z�AځA�z�A�~�AځA�z�A�~�AځA�z�AځA�~�A�|�A�~�A�z�AځA�~�A�|�AڃA�|�A�~�AځA�|�AځA�~�A�~�AځA�~�AځA�|�A�~�AڃA�|�AځAڃA�|�A�~�A�~�A�x�A�~�A�z�A�z�A�~�A�z�A�|�A�|�A�x�A�~�A�v�A�|�A�x�A�z�A�~�A�z�A�~�A�|�A�z�AځA�z�AځA�|�A�|�AځA�z�AځA�|�AځAځA�~�AڅAځA�~�AڃA�~�AڃA�~�AځAڅA�~�AڃAڅA�~�AڃAڅAڃAډ7AڃAڅAڃA�~�AڃA�~�AڃA�|�AڅA�~�AځA�~�A�~�AڅA�~�AڅAڅAڃAڅAڃAډ7AڅAډ7Aڇ+AڅAډ7AڃAڇ+AڃAڅAڇ+AڃAڇ+Aڇ+AڃAڇ+AڅAڅAډ7AځAڅAڅAڃAڇ+AڃAڅAډ7AڅAډ7AڅAډ7AڅAڇ+AڅAڅAڇ+AڅAډ7Aڇ+AڅAڋDAڅAڇ+Aډ7AڅAڋDAڅAډ7Aډ7AڅAڋDAڇ+AڋDAڋDAڋDAڏ\AڑhAڋDAڏ\Aڏ\AڍPAړuAڑhAڍPAڏ\AڋDAڍPAڏ\AڋDAڏ\AڋDAڍPAڑhAڍPAڏ\AڑhAڏ\Aڕ�AړuAڑhAڕ�AړuAڑhAڕ�AړuAڑhAڗ�AڍPAڏ\AڑhAڍPAڍPAڑhAڋDAڍPAڑhAڋDAڑhAڑhAڏ\AڍPAڑhAڍPAڍPAڑhAڍPAڑhAڑhAڍPAڏ\AړuAڏ\Aڏ\AړuAڏ\Aڏ\Aڕ�AڑhAڍPAړuAڑhAڍPAڕ�AڑhAڏ\AړuAڕ�Aڏ\AڑhAړuAڏ\AڍPAڏ\Aڏ\Aڏ\Aڇ+AڅAڇ+AځA�v�A�r�A�n�A�r�A�v�A�n�A�~�A�x�A�v�A�|�A�|�A�|�A�z�A�x�A�x�A�v�A�~�A�n�A�ffA�l�A�t�A�v�A�x�A�z�A�t�A�t�A�x�A�p�A�jA�x�A�n�A�t�A�~�A�x�A�r�A�z�A�z�A�|�A�|�A�~�A�|�A�t�A�r�A�p�A�jA�l�A�hsA�`BA�XA�C�A�M�A�I�A�I�A�A�A�I�A�O�A�C�A�C�A�/A�7LA�1'A�9XA�Q�A�VA�9XA��A� �A� �A�bA��Aٴ9A�x�Aه+A�K�A�M�A�G�A�JAظRA�&�A�A�jA��;A���A�M�A�  A���A���A���A��A���AԶFAԅA�VAӡ�A�Q�A�C�A���Aҝ�A�Q�A�7LA�33A�&�A�%A��`A���A���A�ƨAѺ^Aџ�AуA�l�A�XA�G�A��AЩ�AжFAмjAЇ+A�t�A�XA�E�A�A�A�Aϧ�A�v�A�dZA�I�A�9XA�AζFA�x�A�M�A�=qA�$�A�oA�A��mA��
A���AͮA͏\A�z�A�z�A�hsA�;dA�$�A��#A̺^A̰!A̟�A�E�A�ffA�bAʾwA�jA�ĜA�oA�$�A�v�A�ZA�A�A��A��
A�ƨA��HA��A�oA�M�A�`BA�C�A�XA�XA�/A�
=A��A�A�A���A�ƨA���A���A��A���AƟ�AƅA�~�A�z�A�r�A�C�A��
AŅA� �A��A��A��mA��TA���A�ȴAĴ9Aġ�AąA�&�A�VA�1A�%A�%A���Aß�A�t�A�jA�ZA�-A�VA��mA¬A�S�A�M�A�5?A�-A�-A��A���A��#A��jA���A���A�^5A�1'A� �A�oA�%A���A��mA��TA��
A�ƨA��9A���A���A��\A��PA��PA��A�jA�33A���A�x�A��A���A��+A�;dA�A��\A�=qA�A��wA�XA��mA��jA�jA���A�\)A�ĜA���A���A��uA��DA�z�A�S�A�oA��A�bNA�$�A�1'A�/A�$�A��A�A�  A���A��A��A��A��A��/A���A��
A�ȴA�A�ƨA��PA�Q�A�{A�A��/A��A�^5A�&�A�ȴA�\)A��A���A���A�\)A�{A�G�A��mA���A�&�A�%A�A���A��;A��wA���A�oA��wA���A��PA��A�~�A�x�A�t�A�ffA�`BA�S�A�G�A�E�A�;dA�/A��A��mA���A��wA��A���A���A���A��PA�~�A�n�A�M�A�=qA�1'A�VA��;A��RA���A�r�A�M�A�A�l�A�1A��HA��-A�;dA�oA��A���A��A�I�A� �A�A�A��\A�jA�Q�A�G�A�33A��A��TA���A�$�A�ȴA��DA�XA�+A��A��jA��hA�r�A�l�A�ffA�`BA�\)A�I�A�9XA�$�A���A��`A��FA�/A�l�A�33A��yA���A�r�A�XA�7LA��/A�G�A���A��A��`A��DA�C�A�
=A�A��A�ȴA���A�t�A�;dA�$�A�/A�1'A�$�A�$�A�
=A��7A�;dA�JA��A��A���A�|�A�Q�A�7LA�1A�ƨA���A��PA��PA��\A��A�C�A�5?A�5?A�7LA�7LA�33A�{A��
A���A�z�A�ZA�9XA���A��!A�`BA���A�{A��A��`A���A���A��jA��^A���A��\A��PA��+A�l�A�ZA�E�A�1'A��A�A���A���A���A���A��PA�|�A�hsA�Q�A�;dA�(�A�VA��
A���A�~�A�;d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111133333333333                                                                                                                                                                         111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�XA�bNA�hsA�l�A�t�A�n�A�ffA�r�A�z�A�z�A�|�A�|�A�|�A�~�A�~�A�~�A�|�A�|�A�|�A�~�AځAځAڃAځAځAڅAڅAڅAڅAڇ+Aڇ+Aڇ+Aڇ+AڋDAڏ\AڍPAڏ\AړuAڑhAڏ\Aڏ\AڑhAڑhAڑhAډ7A�t�A�z�A�t�A�t�A�t�A�K�A�5?A�1'Aա�A�A�bA�\)A�p�A�33A�oA�r�A�ȴA�ƨA��A���AƬA�jA�|�Aç�A+A��wA���A��uA��#A�VA��
A���A�  A���A��-A���A�=qA��A�bNA��A�z�A��jA�A� �A�oA�Q�A�bNA�ZA�{A���A�bNA��DA�I�A�r�A�1A�A��
A�v�A��A�=qA��/A���A�ffA��HA�  A��#A�Q�A�9XA��FA�x�A�C�A��^A��A~bA{�Aw�Aq��Ao��AlQ�AfQ�Aax�A_C�A^  A\  AW�mAUt�AR�AP�yAN�DAJI�AG�mAF�yAF �AE�ACXAAdZA@E�A>JA<�!A;�#A;VA:ffA9?}A8��A7�wA5ƨA3;dA2^5A0�jA/�FA/33A.��A-�wA,��A,ZA+7LA*�A)&�A'�;A&A�A$~�A!�AC�A �A
=AhsAS�A1AG�A&�A��AbNAA�jA�A$�A�#AS�A��AI�AVAȴAĜA��AjA��A
jA
=qA
JA
A	��A	l�AbNA��A �A�Ar�Av�AE�A�@�t�@�;d@���@�"�@��@���@�E�@��@���@�"�@�@�=q@�X@�O�@��#@�{@���@���@���@�%@�9X@�%@�n�@�  @�^5@��T@�@�ȴ@���@���@�j@�F@��;@�C�@�33@�|�@�+@�@߾w@�@���@ܼj@�A�@ۥ�@�C�@�n�@ڸR@�`B@��@�G�@��@�J@ٺ^@�I�@�z�@��@��m@ו�@�33@ְ!@Ցh@���@�  @�l�@�n�@�{@�X@ЋD@υ@�o@�@���@�{@�X@��`@˝�@���@ɡ�@�A�@�C�@�V@��
@���@�@�v�@�M�@�{@��#@���@�~�@�  @�K�@���@�9X@��F@���@�`B@�~�@�V@�`B@��@��@���@��u@�  @��@�;d@��+@��@���@�x�@���@���@��@��/@�Z@�1'@�bN@��P@�n�@�X@�G�@���@�p�@�7L@��@��`@�O�@�`B@�G�@��`@���@�b@��;@��w@�\)@�ȴ@��y@�o@���@�J@�G�@��j@�(�@�b@��@�ƨ@��
@�dZ@�+@��H@�v�@�=q@�@��T@�@��h@�`B@�X@�O�@��@��@�bN@�  @��w@�t�@�@�v�@��@��7@�/@�z�@��;@���@��@���@��P@�t�@�33@�
=@��@��\@�n�@�=q@��@�@��-@�/@��@��`@��9@�r�@��@��
@���@��w@��P@�33@���@���@�$�@��h@��@��j@��@�9X@�1@��@��@��\@�v�@�5?@��@��`@��j@���@�bN@�1'@��m@�t�@�S�@�"�@��R@�~�@�J@���@�G�@��@�bN@��
@��@��@��@��@��+@��@��@���@�@�x�@�%@��u@�bN@�bN@�bN@�Q�@�(�@���@�|�@�33@�o@��@���@�v�@�5?@�J@�@��`@��@�(�@�1@�  @��;@��w@��P@�K�@�@��\@�ff@�5?@�{@���@�hs@�G�@��@��/@��j@���@��@�I�@��;@�dZ@�+@�@�ff@�=q@�J@��T@�@��7@�hs@�G�@�7L@�/@�/@�7L@�/@�&�@��@���@��/@��u@�1'@��
@��F@�l�@�33@��@�n�@�-@���@�X@�%@���@�j@�1'@���@�ƨ@��F@��P@�o@��!@�~�@�^5@�5?@���@��#@��^@�x�@�7L@�V@��`@��j@��@��D@�Z@��@�w@�@��@|�@;d@~�@~��@~ff@}�@}?}@|�@|9X@{S�@z��@zJ@y%@x�9@x��@x�u@x�@xb@w�w@w|�@wK�@v��@v�@vff@v$�@v{@u�-@u`B@tZ@s�@rn�@qX@p�u@p  @o�w@r��@q��@q7L@pbN@o��@n�@n�+@m�T@m�@mO�@l�@l��@lI�@l1@kƨ@k33@j�\@j-@i�^@iX@h��@h��@hQ�@hb@g�@gl�@f��@f��@f5?@e@e?}@d��@dZ@d�@c�m@ct�@cS�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�XA�VA�ZA�K�A�XA�bNA�dZA�^5A�dZA�hsA�dZA�jA�hsA�ffA�ffA�bNA�l�A�n�A�v�A�v�A�t�A�v�A�t�A�p�A�r�A�l�A�n�A�hsA�dZA�hsA�ffA�hsA�n�A�r�A�v�A�r�A�z�A�~�A�x�A�~�A�x�A�|�A�|�A�x�A�z�A�~�A�z�A�|�A�~�A�z�A�z�A�~�A�z�A�~�A�z�A�~�A�|�A�z�AځA�z�AځA�z�A�~�AځA�z�A�~�AځA�z�AځA�~�A�|�A�~�A�z�AځA�~�A�|�AڃA�|�A�~�AځA�|�AځA�~�A�~�AځA�~�AځA�|�A�~�AڃA�|�AځAڃA�|�A�~�A�~�A�x�A�~�A�z�A�z�A�~�A�z�A�|�A�|�A�x�A�~�A�v�A�|�A�x�A�z�A�~�A�z�A�~�A�|�A�z�AځA�z�AځA�|�A�|�AځA�z�AځA�|�AځAځA�~�AڅAځA�~�AڃA�~�AڃA�~�AځAڅA�~�AڃAڅA�~�AڃAڅAڃAډ7AڃAڅAڃA�~�AڃA�~�AڃA�|�AڅA�~�AځA�~�A�~�AڅA�~�AڅAڅAڃAڅAڃAډ7AڅAډ7Aڇ+AڅAډ7AڃAڇ+AڃAڅAڇ+AڃAڇ+Aڇ+AڃAڇ+AڅAڅAډ7AځAڅAڅAڃAڇ+AڃAڅAډ7AڅAډ7AڅAډ7AڅAڇ+AڅAڅAڇ+AڅAډ7Aڇ+AڅAڋDAڅAڇ+Aډ7AڅAڋDAڅAډ7Aډ7AڅAڋDAڇ+AڋDAڋDAڋDAڏ\AڑhAڋDAڏ\Aڏ\AڍPAړuAڑhAڍPAڏ\AڋDAڍPAڏ\AڋDAڏ\AڋDAڍPAڑhAڍPAڏ\AڑhAڏ\Aڕ�AړuAڑhAڕ�AړuAڑhAڕ�AړuAڑhAڗ�AڍPAڏ\AڑhAڍPAڍPAڑhAڋDAڍPAڑhAڋDAڑhAڑhAڏ\AڍPAڑhAڍPAڍPAڑhAڍPAڑhAڑhAڍPAڏ\AړuAڏ\Aڏ\AړuAڏ\Aڏ\Aڕ�AڑhAڍPAړuAڑhAڍPAڕ�AڑhAڏ\AړuAڕ�Aڏ\AڑhAړuAڏ\AڍPAڏ\Aڏ\Aڏ\Aڇ+AڅAڇ+AځA�v�A�r�A�n�A�r�A�v�A�n�A�~�A�x�A�v�A�|�A�|�A�|�A�z�A�x�A�x�A�v�A�~�A�n�A�ffA�l�A�t�A�v�A�x�A�z�A�t�A�t�A�x�A�p�A�jA�x�A�n�A�t�A�~�A�x�A�r�A�z�A�z�A�|�A�|�A�~�A�|�A�t�A�r�A�p�A�jA�l�A�hsA�`BA�XA�C�A�M�A�I�A�I�A�A�A�I�A�O�A�C�A�C�A�/A�7LA�1'A�9XA�Q�A�VA�9XA��A� �A� �A�bA��Aٴ9A�x�Aه+A�K�A�M�A�G�A�JAظRA�&�A�A�jA��;A���A�M�A�  A���A���A���A��A���AԶFAԅA�VAӡ�A�Q�A�C�A���Aҝ�A�Q�A�7LA�33A�&�A�%A��`A���A���A�ƨAѺ^Aџ�AуA�l�A�XA�G�A��AЩ�AжFAмjAЇ+A�t�A�XA�E�A�A�A�Aϧ�A�v�A�dZA�I�A�9XA�AζFA�x�A�M�A�=qA�$�A�oA�A��mA��
A���AͮA͏\A�z�A�z�A�hsA�;dA�$�A��#A̺^A̰!A̟�A�E�A�ffA�bAʾwA�jA�ĜA�oA�$�A�v�A�ZA�A�A��A��
A�ƨA��HA��A�oA�M�A�`BA�C�A�XA�XA�/A�
=A��A�A�A���A�ƨA���A���A��A���AƟ�AƅA�~�A�z�A�r�A�C�A��
AŅA� �A��A��A��mA��TA���A�ȴAĴ9Aġ�AąA�&�A�VA�1A�%A�%A���Aß�A�t�A�jA�ZA�-A�VA��mA¬A�S�A�M�A�5?A�-A�-A��A���A��#A��jA���A���A�^5A�1'A� �A�oA�%A���A��mA��TA��
A�ƨA��9A���A���A��\A��PA��PA��A�jA�33A���A�x�A��A���A��+A�;dA�A��\A�=qA�A��wA�XA��mA��jA�jA���A�\)A�ĜA���A���A��uA��DA�z�A�S�A�oA��A�bNA�$�A�1'A�/A�$�A��A�A�  A���A��A��A��A��A��/A���A��
A�ȴA�A�ƨA��PA�Q�A�{A�A��/A��A�^5A�&�A�ȴA�\)A��A���A���A�\)A�{A�G�A��mA���A�&�A�%A�A���A��;A��wA���A�oA��wA���A��PA��A�~�A�x�A�t�A�ffA�`BA�S�A�G�A�E�A�;dA�/A��A��mA���A��wA��A���A���A���A��PA�~�A�n�A�M�A�=qA�1'A�VA��;A��RA���A�r�A�M�A�A�l�A�1A��HA��-A�;dA�oA��A���A��A�I�A� �A�A�A��\A�jA�Q�A�G�A�33A��A��TA���A�$�A�ȴA��DA�XA�+A��A��jA��hA�r�A�l�A�ffA�`BA�\)A�I�A�9XA�$�A���A��`A��FA�/A�l�A�33A��yA���A�r�A�XA�7LA��/A�G�A���A��A��`A��DA�C�A�
=A�A��A�ȴA���A�t�A�;dA�$�A�/A�1'A�$�A�$�A�
=A��7A�;dA�JA��A��A���A�|�A�Q�A�7LA�1A�ƨA���A��PA��PA��\A��A�C�A�5?A�5?A�7LA�7LA�33A�{A��
A���A�z�A�ZA�9XA���A��!A�`BA���A�{A��A��`A���A���A��jA��^A���A��\A��PA��+A�l�A�ZA�E�A�1'A��A�A���A���A���A���A��PA�|�A�hsA�Q�A�;dA�(�A�VA��
A���A�~�A�;d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111133333333333                                                                                                                                                                         111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
��B
�"B
�B
�B
�"B
�WB
�B
��B
�"B
��B
��B
�"B
�"B
�"B
�WB
�WB
��B
��B
�B
�B
�B
��B
�B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�)B
�)B
��B
�)B
��B
�B
�cB
�/B
�/B
��B
�B
��B
�]B�BxB�BܒB%�B^BxBpoBh
BS�B\BTaB��B�:B��B��BרB�BÖB�B�IB�_B�~BzDBp�BgmBd&Bb�Bb�Bo�B|�Bu%BrBiyBaBQB;dB1'B�B iB�]B�wB��Be�Bj�Be,B^�B`�BbBE�B=B9XB4B)�B+B�B
�2B
��B
�B
�&B
��B
�:B
��B
aB
N�B
5B
�B
�B	�rB	�&B	��B	��B	�zB	{B	tB	h
B	o5B	^B	L0B	>�B	4�B	.�B	!�B	 B	
=B	_B	�B	�B�B�|B�WB��B�B�vB�jB�EB�aB��B՛B��BǮBʌB�3BÖB�UB�'B��B�6B�XB��B�-B��B�!B�B�FB��B�B��B��B�hB��B�OB�'B��B��B��B��B�B��B��B�OB��B�RB�nB��B�B��BƨB�^B�UB� B��B�B�dB��B��B��B�,B��B�'B��B�EBĜB��B�FB�BƨB�)BɆB�HB��B�B�*B��B�hB�B�*B��B��B��B��B�,B�`B��B�|B��B	  B	B	�B	xB	MB	 �B	B	VB	�B	B	qB	�B	B	!�B	 �B	�B	=B	�B	�B	�B	�B	�B	FB	+B	B	�B	&LB	(�B	0UB	>B	6zB	8�B	=�B	>B	A�B	C�B	G�B	OB	NpB	M�B	M�B	M�B	MjB	O�B	Q�B	S&B	T,B	U2B	VB	VmB	VmB	W�B	VmB	W?B	Y�B	[�B	[�B	[#B	[�B	[#B	[#B	[�B	\�B	]�B	_;B	aHB	kQB	|�B	��B	~�B	xB	s�B	sB	uZB	�SB	��B	��B	�"B	�.B	��B	��B	��B	��B	��B	�xB	��B	�B	�FB	��B	��B	�OB	��B	�-B	�B	��B	�LB	��B	�OB	��B	�9B	��B	��B	�B	��B	�B	��B	�UB	��B	��B	��B	�OB	��B	� B	��B	�9B	ȴB	ɆB	��B	˒B	��B	˒B	��B	̘B	�jB	�B	ҽB	�TB	��B	��B	�,B	��B	�mB	��B	�EB	�yB	�yB	�EB	�B	�B	چB	�WB	یB	�]B	�]B	��B	ܒB	��B	�5B	�B	�BB	�BB	�B	��B	�B	�B	��B	� B	��B	�2B	�fB	�mB	��B	�>B	�B	�QB	��B	�B	�QB	��B	�)B	�"B	��B	��B	�B	��B	�WB	�B	�B	��B	�iB	�5B	�iB	��B	�oB	�B	�B	��B	�B	�|B	�B	��B	�%B	�%B	��B	��B	��B	�lB	��B	�>B	�xB	��B	��B	��B	�(B	��B	�.B	�cB	�cB	��B
 iB
 �B
�B
AB
AB
uB
AB
�B
�B
MB
�B
�B
�B
�B
B
SB
+B
+B
�B
_B
�B
1B
�B
�B
�B
DB
�B
�B
B
B
�B
�B
"B
VB
�B
bB
�B
 B
 B
B
�B
oB
�B
@B
@B
uB
uB
�B
B
MB
�B
�B
$B
�B
YB
�B
�B
�B
_B
�B
�B
�B
�B
�B
_B
�B
�B
�B
1B
B
�B
qB
=B
B
B
CB
~B
B
IB
�B
�B
B
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
 �B
!-B
 �B
!-B
!�B
"4B
"4B
"�B
#B
#:B
#�B
$�B
&B
%�B
%�B
%�B
%�B
&B
&�B
&�B
&�B
&�B
'RB
'�B
'�B
(XB
(�B
)�B
*eB
)�B
)�B
)�B
)�B
*0B
*eB
*0B
*eB
*eB
*0B
+6B
+6B
+B
+6B
+6B
,B
,B
,�B
-wB
.B
.IB
.B
-wB
.B
.IB
.�B
/B
/�B
/�B
0!B
0UB
0�B
0�B
1'B
1�B
1�B
1�B
2�B
3hB
3hB
3�B
49B
4nB
4�B
5B
5?B
5�B
6B
6�B
6�B
7�B
8B
8�B
8�B
9$B
9$B
9XB
9�B
9�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�iB
��B
��B
�B
�]B
�QB
�DB
��B
�QB
�QB
�)B
�B
�WB
�WB
�B
�B
�B
�)B
�B
�"B
�WB
�B
�"B
�WB
�B
�]B
�B
�B
��B
�B
�WB
��B
�B
��B
�B
�B
�WB
�B
��B
�B
�)B
�B
�B
��B
�B
��B
�B
��B
��B
�B
�"B
��B
�]B
�QB
�)B
�QB
�B
�B
��B
�)B
�B
��B
��B
�QB
�)B
�"B
�B
�)B
��B
�WB
�B
�B
�]B
��B
�WB
��B
��B
�)B
�WB
�B
�]B
�B
�B
��B
�B
�WB
�QB
�]B
��B
�B
�)B
��B
�B
�B
�B
��B
�)B
�B
�)B
�B
�B
��B
�B
�B
��B
�B
�]B
�B
��B
��B
�B
��B
�B
�WB
��B
��B
��B
�B
�WB
�"B
��B
�]B
�B
�WB
�QB
�B
�"B
�KB
�B
�"B
�B
�"B
�B
�WB
��B
�B
��B
�B
��B
��B
�B
�B
�B
�B
��B
�B
��B
�B
�B
�B
��B
�/B
�B
��B
�B
�B
�]B
�"B
��B
�B
�B
�]B
��B
�B
�B
�B
��B
��B
�]B
�B
�B
��B
�]B
��B
�"B
��B
�"B
�B
��B
�"B
��B
�B
�B
�/B
�B
�B
�B
�WB
��B
��B
�"B
��B
��B
�B
�B
�B
�WB
�]B
�]B
�WB
��B
�"B
��B
��B
�B
�B
��B
�WB
��B
�B
��B
��B
�WB
��B
�B
��B
�B
�WB
��B
�"B
�B
��B
�B
�B
��B
�B
�B
�B
�B
�/B
��B
�B
�B
�"B
��B
�)B
��B
��B
�B
��B
��B
�WB
�B
�)B
�QB
��B
��B
�"B
�)B
��B
�QB
�cB
��B
�"B
��B
�)B
�"B
��B
�]B
�B
�/B
�WB
�"B
�)B
��B
�WB
��B
��B
��B
�cB
�B
�WB
��B
�)B
�B
��B
�)B
�"B
�B
��B
�B
�]B
�cB
��B
��B
�/B
��B
��B
��B
�B
�"B
��B
�)B
�WB
�/B
�B
�"B
�B
�B
��B
�/B
�B
��B
�B
�)B
�B
��B
�)B
�5B
�B
��B
�)B
�]B
�B
�)B
��B
�B
�iB
�]B
��B
�B
��B
�B
��B
�5B
��B
�B
� B
�cB
�B
�B
�B
��B
��B
�WB
��B
��B
��B
�B
�WB
�)B
��B
�WB
�)B
�QB
�cB
�B
�B
�cB
�B
�/B
�;B
�B
�5B
�B
� B
�;B
�QB
��B
�TB
�iB
�B
��B
�B
��B
�cB
�B
��B
�B
�
B
�B
��B
�B
�B
�B
��B
��B
��B
��BB�B0�B=qB:�BK�B{JB��B�bB�B�B��B�\B�xB�B�=B�'B�tB�B��B��BϫB�9B�?B�9B��B��B�B��B��B 4BB�B �B,=B4B:*BGB7�B6�BXBdZBc�BjBd�Bd&Bt�B|Br�Bs�Bv�Bt�BxB�Bz�BwfBq�Br�Bo BncBm)BkBhsBjBf�Bd�B`vBg�Bj�Bj�BjKB_;B\�BY�Bh
BYBB�B;0B7LBB'B7�B2�B �B�+B��B��BuB�B,B6FB<�Bd�B|�B��B��B�=B��B��B�B��B��B�FB��B��B��B�VB�@B�6B��B��B��B��B��B�B�B��B��B��B��B�B�9B�BȴB��BуB�B��B�B՛B�aB�
B�BӏBҽB՛B��B҉B�B�QB� B��B��B�KBŢB��B�^BȀB��B��B��B�KB�*B�*B�nB�?B��B�B��B��B��B��B�B��B�wB�IB�wB�CB��B��B�'B�[B�_B�IB��B��B��B��B��B�VB�xB��B�.B}�B�B�~B~�B��Bt�BqvBqABrBsBt�Br�Bq�B�~BqABh
Bh>Bh
Bk�BiyBg8Bh�Bd�Bd�Bf�Be,BffBd&Bd�Be�B_�B_pBa�BgBd�B]/Bd�Bg�B^�B`BBg�Bg8B]�Ba�B[�BYBbBv`BcTBc�ByrBo5Bo Bq�Bu�BuZB}VB�7B�4BxlBw2BwfBu�Bt�Bv+Bv�Bs�Bv+Bt�Bs�BsMBr�Bu�Bw�Bo�Bm�BncBm�BjBi�BkBiDBh>BkQBc�Be,Bd&BgB^B`vB[WB[�B`�Bf�BM�BK�BM6BTaB?�BGEB@B;0B>�B9$B7�B;dB7LB6�B2�B.�B0!B0UB1�B.IB6B%zB�B�B�B�B�B
=B�BB;B��B��B�xB�>B�%B�TB��B�TB�fB��B��B�B�mB�HB��B��B�6B��B�B��B��B��By�Bs�BjBj�Bl"Be�Bh�Bi�B^B]�Bh
Be`Be`Bm)B�Bq�BffB`�Bf�BhsBe�Ba�B^�Be,Be,B`B[WB\]B[�B_�B_B`BB_;B^jB[�B_;Bf�Bh�B_�B^�B\�B\)B_Bb�BaHBo�BV9BF�BJ�BC-BB�B?�BB[B?HBB'B;dB:�B=qB=�B=�B9�B7LB6FB6B4nB7�B=B@�B7�B7B4B1�B1'B1�B4�B.�B)*B/�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                         444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B
��B
�>B
�rB
��B
�
B
�rB
�B
�
B
�>B
�rB
�>B
�>B
�rB
�rB
�rB
�B
�B
�>B
�>B
�
B
��B
��B
�>B
��B
�DB
��B
�B
�B
�DB
�DB
�DB
�B
�DB
��B
�B
�DB
�DB
�B
�DB
�yB
�yB
�DB
�yB
�DB
��B
�B
�B
�B
�JB
��B
�B
�B�BtSB�XB��B!�BZQBtSBl�BdZBO�B�BP�B�FB��B��B�)B��B�^B��B�UB��B��B��Bv�Bl�Bc�B`vB_;B_Bl"Bx�BquBncBe�B]cBMjB7�B-wB*B��B�B��B�MBbBg8Ba|B[#B\�B^iBB&B9XB5�B0UB&LB{B
��B
�B
�"B
�B
�vB
�KB
��B
��B
]cB
K)B
1[B
$B
:B	��B	�vB	�B	�BB	��B	wfB	poB	dZB	k�B	ZQB	H�B	:�B	0�B	+6B	OB	PB	�B	�B	  B�(B�iB��B�B�B��B��BںBԕBбB�BB��B�KB��B��B��B��B��B�wB�)B��B��B�9B�}B�B�qB�XB��B��B�kB��B�B��B�7B��B�wB��B��B�'B�B�aB�IB�6B��B�B��B��B�?B�jB��B��BǮB��B�pB��B�aBȴB�/B�<B�BB�|B�#B�wB�BÕB��B�EB��B�[B��B�yB��B��B��B�jB�zB��B��B�gB�zB� B�B�EB�B�|B�B�2B��B�NB�PB	oB�.B	�B	 �B��B�\B	
�B	IB	RB	�B	�B	eB	B	IB	=B	�B	�B	B	B	:B	B	�B	{B	_B	B	"�B	%FB	,�B	:^B	2�B	5?B	9�B	:^B	>BB	?�B	C�B	K^B	J�B	J#B	J#B	J#B	I�B	K�B	M�B	OvB	P|B	Q�B	RTB	R�B	R�B	T,B	R�B	S�B	V8B	XEB	XB	WsB	XEB	WsB	WsB	XEB	YB	Y�B	[�B	]�B	g�B	y	B	~(B	{B	tSB	p;B	oiB	q�B	��B	�+B	�7B	�rB	�~B	��B	��B	�.B	��B	�FB	��B	�B	�UB	��B	�B	��B	��B	��B	�}B	�UB	�B	��B	�OB	��B	�<B	��B	�-B	�3B	�gB	�?B	�^B	�B	��B	�B	�<B	�B	��B	�6B	�pB	�B	��B	�B	��B	�EB	��B	�B	��B	�B	��B	ɺB	�^B	�B	ΤB	�<B	�HB	�|B	� B	ҽB	�&B	ԕB	��B	��B	ԕB	�gB	�mB	��B	קB	��B	حB	حB	�B	��B	�KB	څB	��B	ܒB	ܒB	��B	�/B	��B	�cB	�5B	�pB	�GB	�B	�B	�B	�%B	�B	��B	�B	�8B	�lB	�B	�>B	�yB	�rB	�>B	�>B	�
B	�>B	�B	��B	��B	�B	�B	�B	�B	�"B	�B	��B	��B	�(B	��B	��B	��B	�B	�uB	�uB	�B	�GB	�B	��B	��B	��B	��B	��B	�DB	�B	�xB	�DB	�~B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�.B
  B
 �B
 �B
 �B
 �B
 �B
oB
�B
{B
{B
GB
�B
B
�B
MB
B
�B
�B
1B
	7B
	kB
	kB

	B

=B

rB

�B
B
�B
�B
PB
PB
VB
�B
�B
'B
�B
�B
�B
�B
�B
hB
�B
B
:B
tB
@B
�B
�B
B
�B
�B
�B
B
B
B
�B
�B
B
�B
B
�B
RB
$B
�B
�B
_B
_B
�B
�B
eB
�B
B
�B
kB
�B
=B
B
=B
=B
=B
�B
IB
B
IB
IB
}B
IB
}B
�B
�B
�B
!B
UB
�B
�B
 �B
"hB
!�B
"3B
"3B
"3B
"hB
"�B
"�B
"�B
#9B
#�B
#�B
$@B
$�B
%B
%�B
&�B
&LB
&LB
&LB
&B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'RB
'�B
'�B
(XB
(XB
)*B
)�B
*dB
*�B
*dB
)�B
*dB
*�B
+6B
+kB
+�B
,B
,qB
,�B
,�B
-B
-wB
-�B
.B
.IB
.�B
/�B
/�B
0 B
0�B
0�B
0�B
1[B
1�B
2-B
2aB
2�B
33B
3�B
4mB
4�B
5?B
5tB
5tB
5�B
6EB
6G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�>B
�B
�WB
�B
�B
�B
�8B
�B
�B
�yB
�lB
�B
�B
�lB
��B
�fB
�yB
�B
�rB
�B
�lB
�rB
�B
�lB
�B
��B
�
B
�B
��B
�B
�B
��B
�>B
�
B
��B
�B
�lB
�B
�lB
�yB
��B
�
B
�B
��B
�8B
��B
�>B
�8B
��B
�rB
�8B
�B
�B
�yB
�B
�
B
��B
�8B
�yB
�lB
�DB
�>B
�B
�yB
�rB
��B
�yB
�8B
�B
��B
�
B
�B
�8B
�B
�DB
�8B
�yB
�B
�lB
�B
��B
��B
�DB
��B
�B
�B
�B
�B
��B
�yB
�>B
�lB
��B
��B
�>B
�yB
�lB
�yB
��B
�B
�B
��B
�
B
�DB
�B
�B
�lB
�DB
�B
�B
�DB
�lB
�B
�B
�8B
�DB
�B
�B
�rB
�8B
�B
�B
�B
�B
�lB
�rB
�B
�
B
�rB
��B
�rB
��B
�B
�>B
��B
�B
�lB
�8B
�B
�lB
�lB
��B
�B
�B
�
B
�>B
��B
�
B
��B
�>B
�B
��B
�JB
��B
��B
�B
�rB
�JB
�
B
��B
�B
�>B
��B
��B
��B
�>B
�B
�B
��B
��B
�>B
�B
�B
�rB
�B
�rB
��B
�B
�rB
�B
��B
�
B
�B
��B
��B
��B
�B
�B
�DB
�rB
�B
�>B
��B
��B
��B
�B
�B
�B
�B
�B
�rB
�B
�B
��B
��B
�DB
�B
�JB
��B
�B
�B
�B
�B
�
B
�B
��B
�B
�B
�rB
��B
�JB
��B
�
B
�B
��B
��B
��B
��B
�B
�B
��B
��B
�rB
�JB
�yB
�>B
�JB
��B
�>B
�B
�B
��B
�yB
�B
�DB
�JB
�rB
�yB
�JB
�B
�B
�DB
�rB
�B
�yB
�rB
�JB
�B
��B
�B
�B
�rB
�yB
�JB
�B
�B
�JB
�>B
�B
��B
�B
�JB
�yB
�
B
�B
�yB
�rB
��B
�B
�
B
�B
�B
�>B
�B
�B
�>B
�DB
�JB
��B
�rB
�B
�yB
�B
�B
��B
�rB
��B
��B
�DB
�B
�lB
�B
��B
�yB
�WB
�DB
�yB
�B
�
B
�B
�yB
�B
��B
�yB
�"B
��B
�B
�B
�8B
� B
�(B
�
B
�JB
�B
�JB
��B
�PB
�B
��B
��B
��B
�B
�B
�B
�B
�B
�(B
��B
�B
�yB
�JB
�B
�yB
�B
�B
��B
��B
�B
��B
�B
�B
�B
�B
��B
�PB
�B
�B
�8B
�B
�B
��B
�JB
��B
�(B
�B
��B
�;B
�WB
�ZB
��B
�>B
��B
�cB
�lB
��B
�DB
�7B
�DBbBB,�B9�B7BG�Bw�B.B��B�_B�eB�%B��B��B�eB��B�wB��B�jB�BB�HB��B��BB��B�9B� B��B�B�>B��BoBBB(�B0UB6zBCaB49B33BT`B`�B`ABffB`�B`vBqABxlBo5Bo�BsMBqBtSB}VBv�Bs�Bm�Bo BkPBj�BiyBglBd�Bf�Bb�BaGB\�Bd%Bg8BgBf�B[�BYKBVBdZBU�B?B7�B3�B>wB49B.�B��B�{B�;B��B��B�B(XB2�B8�BaGBy>B�IB��B��B�=B��B�RB��B�!B��B��B��B��B��B��B��B�9B��B�B�B�IB�[B�^B�#B�'B��B�0B�jB��B�aB�B�KB��B��B�NB�TB��BбB�ZB�B��B�B��B�EB��B�WB֡B�pB�?B�?BěB��B�,BǮB��B�B�6B�)BěB�zB�zB��B��B��B�UB�<B�BB�0B�0B�^B��B��B��B��B��B�OB��B�wB��B��B��B�3B��B��B�7B�LB��B��B��B�~By�B}VB��B{B�BqABm�Bm�BncBoiBp�Bo5Bm�B��Bm�BdZBd�BdZBh>Be�Bc�Be,BaBaBb�Ba|Bb�B`vB`�BbB\)B[�B^5BcTB`�BYBaGBc�B[#B\�Bc�Bc�BZB^BXBU�B^iBr�B_�B_�Bu�Bk�BkPBn.BrBq�By�B��B|�Bt�Bs�Bs�BrBqABr{BsMBo�Br{BqABo�Bo�Bo5Bq�Bs�Bk�BjJBj�BjBffBf2BglBe�Bd�Bg�B`ABa|B`vBcTBZQB\�BW�BW�B]/Bb�BI�BG�BI�BP�B<BC�B<jB7�B:�B5tB49B7�B3�B33B.�B+B,qB,�B-�B*�B2aB!�B�B�B@B�B
=B�B:B�VB��B��B�7B��B��B�uB�B�"B�B�B�,B�HB�dB��B��B��B�EB��B��B�XB��B��B�:Bv+Bo�Bf�BgBhrBbNBd�Be�BZQBZBdZBa�Ba�BiyB{�Bm�Bb�B]/BcBd�BbB^5B[#Ba|Ba|B\]BW�BX�BW�B[�B[WB\�B[�BZ�BXB[�BcBe,B\)B[#BYKBXyB[WB_B]�Bl"BR�BC,BF�B?}B?HB<6B>�B;�B>wB7�B6�B9�B:)B:)B5�B3�B2�B2aB0�B4B9XB=B49B3gB0UB.IB-wB.B0�B+B%zB,44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                         444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721225013                            20230721225013AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122501320230721225013  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122501320230721225013QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122501320230721225013QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8800            800             