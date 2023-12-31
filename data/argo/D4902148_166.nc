CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-03-04T12:36:59Z creation;2019-03-04T12:37:03Z conversion to V3.1;2019-12-18T07:16:37Z update;2022-11-21T05:29:18Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z          :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   B(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       D,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       N8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   V@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       XD   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `L   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       bP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   jX   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       l\   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   td   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       vh   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       ~p   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       �x   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20190304123659  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_166                     2C  Dd�NAVIS_A                         0397                            ARGO 011514                     863 @ج`� 1   @ج�s�@<���҉�d�E��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@���A   A   A@  A`  A�  A�33A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�R@}p�@�Q�@��AA=A]A}A�{A��HA��HA��HA��HA߮A��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_�
Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
D �
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
D	w
D	�
D
w
D
�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
D w
D �
D!w
D!�
D"w
D"�
D#w
D#�
D$w
D$�
D%w
D%�
D&w
D&�
D'w
D'�
D(w
D(�
D)w
D)�
D*w
D*�
D+w
D+�
D,w
D,�
D-w
D-�
D.w
D.�
D/w
D/�
D0w
D0�
D1w
D1�
D2w
D2�
D3w
D3�
D4w
D4�
D5w
D5�
D6w
D6�
D7w
D7�
D8w
D8�
D9w
D9�
D:w
D:�
D;w
D;�
D<w
D<�
D=w
D=�
D>w
D>�
D?w
D?�
D@w
D@�
DAw
DA�
DBw
DB�
DCw
DC�
DDw
DD�
DEw
DE�
DFw
DF�
DGw
DG�
DHw
DH�
DIw
DI�
DJw
DJ�
DKw
DK�
DLw
DL�
DMw
DM�
DNw
DN�
DOw
DO�
DPw
DP�
DQw
DQ�
DRw
DR�
DSw
DS�
DTw
DT�
DUw
DU�
DVw
DV�
DWw
DW�
DXw
DX�
DYw
DY�
DZw
DZ�
D[w
D[�
D\w
D\�
D]w
D]�
D^w
D^�
D_w
D_�
D`w
D`�
Daw
Da�
Dbw
Db�
Dcw
Dc�
Ddw
Dd�
Dew
De�
Dfw
Df�
Dgw
Dg�
Dhw
Dh�
Diw
Di�
Djw
Dj�
Dkw
Dk�
Dlw
Dl�
Dmw
Dm�
Dnw
Dn�
Dow
Do�
Dpw
Dp�
Dqw
Dq�
Drw
Dr�
Dsw
Ds�
Dtw
Dt�
Duw
Du�
Dvw
Dv�
Dww
Dw�
Dxw
Dx�
Dyw
Dy�
Dzw
Dz�
D{w
D{�
D|w
D|�
D}w
D}�
D~w
D~�
Dw
D�
D�;�D�{�D��R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111ĜA��!A��9A��!A��9A��9A��RA��RA��RA��FA��RA��jA��jA��wA���A���A���A���A�A�ĜA�ĜA���A�A�A��jA��jA���A�A�A�A�ĜA�A�ĜA�ĜA�ĜA�ȴA���A���A���A�A��FA��!A��wA�jA�%A�bA� �A�hsA��A�ĜA��PA�7LA���A�ȴA��A�ffA�A�A�%A��A�;dA�1A��jA�bNA��
A���A�?}A�1A�ĜA�{A���A�ƨA��A��PA�Q�A���A�?}AS�A~I�A}l�A|�A{x�Az�AzI�AyAydZAx�/Ax(�Awp�Av�jAu|�AtI�Ar�!AqK�ApE�AoAo��Ao;dAnI�Am33Ak;dAh��Ag�Af�+Ad�!AbffAa33A`�jA_A^��A\��AZ�!AX��AW
=AU��AUAS�PARZAP�`AO�wAM�AM��AL1'AJ��AH��AH5?AG�#AG|�AGK�AF��AE��AE|�AEl�AD��ADJACXAB�DAA��AAO�AAVA@��A?O�A>JA=`BA=&�A<�\A;l�A:��A:5?A9��A8~�A7��A6~�A5�-A5x�A5dZA5S�A5?}A4�RA3dZA2��A2bA1��A1/A0ȴA0ffA/l�A/oA.��A.��A-�A-��A,�yA+�A+O�A+;dA*�yA*M�A(�/A'�A'��A'XA&��A%�TA$I�A#�A#G�A"��A"JA!�A!��A!�-A!|�A!"�A VA 1A�FAx�AS�A�Ap�A�/An�A��A`BA1AdZA��AbNA�^A�A5?A�hA`BA�HA�RA�+AbA�yA=qA�wA�hAt�A+A�;A�DAx�A��A
5?A	�-A	C�AbNA�HA �A��Al�A/A��A�#A��A�A?}A�A�A��A�DA�
A ��@��
@�o@��@�x�@���@���@�33@��@�ff@�/@� �@�
=@���@�V@�P@�E�@�I�@�@�-@�/@��m@�@�5?@��`@�  @�hs@���@��m@��H@�V@���@�?}@���@�S�@�C�@ݡ�@��/@�b@�=q@�hs@ؓu@��H@�z�@�ȴ@��@Ѳ-@���@�I�@�dZ@�{@��
@��@�b@�v�@�Q�@��-@�(�@��@��7@�b@�dZ@�
=@�@�@���@�$�@��@�j@���@��@�`B@��@���@�9X@�(�@� �@�b@��;@���@�"�@��R@��\@�{@���@��@��@�dZ@�"�@���@��@���@���@�b@�33@�n�@���@��y@��\@�n�@�ff@�-@�@��#@�@���@��h@�x�@�O�@�&�@���@��/@�Ĝ@��@�X@�  @�S�@�o@��H@���@�E�@���@�p�@�`B@��@�Z@���@��H@�M�@�x�@���@�r�@��@��w@���@�S�@�v�@���@��\@��+@�v�@�V@�5?@��7@���@�Q�@� �@��@�l�@�n�@�=q@��@�{@��@��-@�7L@��D@�I�@��m@��;@��@��@�^5@���@�`B@���@��9@��u@�Z@�1@��m@��w@���@�@��R@���@�V@�=q@��@���@���@��-@���@��7@�p�@�`B@�/@��D@�o@���@�5?@���@�/@��@�Ĝ@���@��@�Z@�Q�@�(�@��m@�\)@�+@��@���@�^5@�J@���@�@��^@�?}@�9X@�b@�1@�  @�w@K�@~�y@~E�@}��@}��@}�@|�@{�F@z�!@z=q@y�#@y�7@x�`@x�@xbN@xQ�@xA�@x1'@x �@w�;@w|�@w
=@v��@vV@uV@t�D@t�@s��@s��@s�m@s�m@s�m@s�m@s�
@s�
@s�F@st�@r�@qG�@o�@n�y@nff@n5?@n$�@n@m�-@m�@l�@lZ@l1@kS�@j~�@i��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111ĜA��!A��9A��!A��9A��9A��RA��RA��RA��FA��RA��jA��jA��wA���A���A���A���A�A�ĜA�ĜA���A�A�A��jA��jA���A�A�A�A�ĜA�A�ĜA�ĜA�ĜA�ȴA���A���A���A�A��FA��!A��wA�jA�%A�bA� �A�hsA��A�ĜA��PA�7LA���A�ȴA��A�ffA�A�A�%A��A�;dA�1A��jA�bNA��
A���A�?}A�1A�ĜA�{A���A�ƨA��A��PA�Q�A���A�?}AS�A~I�A}l�A|�A{x�Az�AzI�AyAydZAx�/Ax(�Awp�Av�jAu|�AtI�Ar�!AqK�ApE�AoAo��Ao;dAnI�Am33Ak;dAh��Ag�Af�+Ad�!AbffAa33A`�jA_A^��A\��AZ�!AX��AW
=AU��AUAS�PARZAP�`AO�wAM�AM��AL1'AJ��AH��AH5?AG�#AG|�AGK�AF��AE��AE|�AEl�AD��ADJACXAB�DAA��AAO�AAVA@��A?O�A>JA=`BA=&�A<�\A;l�A:��A:5?A9��A8~�A7��A6~�A5�-A5x�A5dZA5S�A5?}A4�RA3dZA2��A2bA1��A1/A0ȴA0ffA/l�A/oA.��A.��A-�A-��A,�yA+�A+O�A+;dA*�yA*M�A(�/A'�A'��A'XA&��A%�TA$I�A#�A#G�A"��A"JA!�A!��A!�-A!|�A!"�A VA 1A�FAx�AS�A�Ap�A�/An�A��A`BA1AdZA��AbNA�^A�A5?A�hA`BA�HA�RA�+AbA�yA=qA�wA�hAt�A+A�;A�DAx�A��A
5?A	�-A	C�AbNA�HA �A��Al�A/A��A�#A��A�A?}A�A�A��A�DA�
A ��@��
@�o@��@�x�@���@���@�33@��@�ff@�/@� �@�
=@���@�V@�P@�E�@�I�@�@�-@�/@��m@�@�5?@��`@�  @�hs@���@��m@��H@�V@���@�?}@���@�S�@�C�@ݡ�@��/@�b@�=q@�hs@ؓu@��H@�z�@�ȴ@��@Ѳ-@���@�I�@�dZ@�{@��
@��@�b@�v�@�Q�@��-@�(�@��@��7@�b@�dZ@�
=@�@�@���@�$�@��@�j@���@��@�`B@��@���@�9X@�(�@� �@�b@��;@���@�"�@��R@��\@�{@���@��@��@�dZ@�"�@���@��@���@���@�b@�33@�n�@���@��y@��\@�n�@�ff@�-@�@��#@�@���@��h@�x�@�O�@�&�@���@��/@�Ĝ@��@�X@�  @�S�@�o@��H@���@�E�@���@�p�@�`B@��@�Z@���@��H@�M�@�x�@���@�r�@��@��w@���@�S�@�v�@���@��\@��+@�v�@�V@�5?@��7@���@�Q�@� �@��@�l�@�n�@�=q@��@�{@��@��-@�7L@��D@�I�@��m@��;@��@��@�^5@���@�`B@���@��9@��u@�Z@�1@��m@��w@���@�@��R@���@�V@�=q@��@���@���@��-@���@��7@�p�@�`B@�/@��D@�o@���@�5?@���@�/@��@�Ĝ@���@��@�Z@�Q�@�(�@��m@�\)@�+@��@���@�^5@�J@���@�@��^@�?}@�9X@�b@�1@�  @�w@K�@~�y@~E�@}��@}��@}�@|�@{�F@z�!@z=q@y�#@y�7@x�`@x�@xbN@xQ�@xA�@x1'@x �@w�;@w|�@w
=@v��@vV@uV@t�D@t�@s��@s��@s�m@s�m@s�m@s�m@s�
@s�
@s�F@st�@r�@qG�@o�@n�y@nff@n5?@n$�@n@m�-@m�@l�@lZ@l1@kS�@j~�@i��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�!B�'B�'B�'B�'B�'B�!B�!B�!B�!B�!B�'B�'B�'B�'B�'B�'B�'B�'B�'B�'B�'B�'B�'B�!B�!B�!B�B�!B�!B�!B�!B�!B�!B�B�!B�!B�!B�!B�B�B�B�B��B�BVBA�B2-B%�BPB�ZB�)B��B�!B��B��Bt�B[#BP�B8RB�BB
��B
�B
�/B
�B
��B
��B
ÖB
�dB
�!B
��B
��B
��B
��B
�=B
�B
z�B
t�B
o�B
gmB
bNB
_;B
[#B
XB
S�B
N�B
I�B
C�B
:^B
1'B
%�B
�B
�B
�B
{B
hB
DB
B	�B	�
B	��B	ƨB	�jB	�B	��B	��B	��B	�bB	�B	s�B	e`B	[#B	T�B	N�B	C�B	;dB	49B	-B	"�B	!�B	�B	bB	
=B	+B	B	B	B	  B��B��B��B��B�B�B�B�B�B�yB�sB�`B�HB�HB�BB�5B�B�
B��B��B��B��BȴBɺBɺBɺBȴBǮBȴBȴBȴBŢBÖB��B��B�}B�qB�jB�dB�^B�RB�FB�9B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�oB�hB�\B�VB�PB�JB�=B�+B�B�B� B}�B{�Bx�Bv�Bu�Bs�Br�Bo�Bm�Bk�Bk�BjBiyBhsBffBcTBbNB`BB`BB_;B^5B[#BW
BR�BQ�BS�BR�BQ�BN�BL�BK�BJ�BJ�BI�BH�BG�BG�BF�BF�BF�BF�BE�BD�BC�BB�BA�B@�B?}B>wB=qB=qB=qB<jB<jB<jB;dB;dB:^B9XB8RB8RB8RB7LB7LB7LB7LB6FB6FB6FB5?B6FB5?B5?B5?B5?B5?B5?B5?B5?B49B5?B5?B49B49B33B2-B1'B33B49B49B49B49B49B33B33B5?B49B7LB7LB9XB<jB>wB>wBB�BD�BD�BE�BG�BH�BI�BI�BH�BH�BH�BN�BO�BQ�BR�BT�BT�BVBVBW
BXBYBZBZB\)B]/B_;BbNBdZBe`BffBgmBhsBjBm�Bo�Bq�Bx�B~�B� B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�DB�VB�hB�oB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�?B�FB�FB�LB�XB�jB�dB��BBÖBŢBǮBǮBǮBǮBƨBȴB��B��B��B��B��B��B�B�B�5B�HB�TB�`B�fB�mB�sB�yB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B	+B	
=B	VB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	%�B	'�B	'�B	'�B	+B	49B	9XB	;dB	;dB	=qB	>wB	?}B	?}B	@�B	@�B	B�B	D�B	H�B	M�B	O�B	Q�B	S�B	VB	XB	XB	XB	XB	ZB	ZB	\)B	]/B	_;B	aHB	cTB	ffB	hsB	jB	l�B	l�B	m�B	m�B	m�B	m�B	n�B	n�B	n�B	o�B	p�B	v�B	z�B	|�B	}�B	~�B	~�B	~�B	~�B	�B	�%B	�=B	�JB	�JB	�PB	�V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�;B�AB�AB�AB�AB�AB�;B�;B�;B�;B�;B�'B�AB�'B�AB�AB�'B�'B�'B�'B�AB�'B�'B�'B�!B�;B�!B�B�;B�;B�!B�!B�;B�;B�B�;B�;B�UB�;B�OB�IB�wB��B�8B�BX_BCaB4B)*BhB�B�5BЗB�aB�B��Bw�B\�BT�B<�B�B�B;B
��B
�jB
��B
��B
�\B
�B
�VB
��B
�>B
��B
��B
�?B
��B
�AB
|B
u�B
qB
hXB
cB
_�B
[�B
X�B
T�B
O�B
J�B
E9B
<B
3B
'�B
�B
1B
�B
2B
�B
B
�B	�UB	��B	ϑB	�B	��B	��B	��B	�BB	�QB	��B	��B	v+B	gmB	\�B	VmB	P�B	ESB	=<B	5�B	.�B	#�B	#�B	�B	TB	)B	�B	�B	�B	�B	B�0B�dB��B��B�B��B�B�B�B�B�0B��B�NB��B�bBߤB�B�B�B�[B�B�pBɠB�	B�	B�	B�7BȴB�XBɺBɆB�YB�gB�;B�;B��B�B��B�B�JB�	B�fB�tB�B��B��B�/B��B�6B�fB��B��B�TB��B�qB�QB�yB�YB��B��B��B��B�&B�TB��B��B��B��B��B��B��B��B��B.B}�By�Bw�Bv�Bt�Bs�Bp�Bn}Bl"Bl"BkBjBiyBg�Bd@BcB`�B`�B`BB`'B\�BX�BUgBS�BT�BS�BSuBP�BM�BL~BKDBK^BJ�BI�BH1BHBGBF�BGBG+BFYBE�BE9BC�BB'BAUB@B?.B>]B=�B=�B="B=VB=<B<jB<jB;JB:�B9�B9�B9$B8lB8B88B7�B7fB7fB7LB6�B6�B5�B5�B5�B5�B5�B6B5�B5�B5tB6B6B5�B4�B49B3�B2�B4nB4�B4�B4�B4�B5%B4nB4�B6`B6+B8�B9	B;B=�B?cB?�BC�BEBEBE�BG�BIBJ=BJrBI�BI�BI�BOBBPbBR:BS[BU2BUBVSBVmBWsBXyBYeBZkBZ�B\�B]�B_�Bb�Bd�Be�Bf�Bg�Bi*Bk6BncBp�BsBy�BHB�OB�;B�UB�UB�[B�AB�-B�-B�GB�GB�MB�gB�gB��B�B��B�0B��B��B��B��B��B��B�B��B�B�jB�vB�bB�nB��B�nB�&B�@B�FB�2B�sB��B�5B�MB�ZB�zB��B��B��B��B��B��B��B�B�YB��B��B��B��B�B�B�^B�6B�BB�:B�TBԕBؓBڠBޞB�B�B�B�B�B�B��B��B�B��B��B��B�B��B��B��B��B��B�B��B�B�2B��B�B	�B	�B	
�B	�B	�B	�B	�B	�B	�B	�B	�B	B	#B	B	B	!B	"4B	$&B	&2B	($B	(>B	(sB	+�B	4nB	9�B	;B	;�B	=�B	>�B	?�B	?�B	@�B	@�B	B�B	EB	I7B	N"B	P.B	R B	TFB	VSB	XEB	XEB	XEB	X+B	ZQB	ZkB	\xB	]dB	_�B	a�B	c�B	f�B	h�B	j�B	l�B	l�B	m�B	m�B	m�B	m�B	n�B	n�B	n�B	pB	qAB	wLB	{0B	}<B	~B	.B	.B	HB	.B	�GB	�tB	��B	��B	��B	��B	�p1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.14(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201903150031552019031500315520190315003155202211182138132022111821381320221118213813201903160019062019031600190620190316001906  JA  ARFMdecpA19c                                                                20190304213634  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190304123659  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190304123701  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190304123702  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190304123702  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190304123702  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190304123702  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190304123702  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20190304123703                      G�O�G�O�G�O�                JA  ARUP                                                                        20190304125642                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190304153320  CV  JULD            G�O�G�O�F�`�                JM  ARCAJMQC2.0                                                                 20190314153155  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190314153155  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190315151906  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114231518                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123813  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                