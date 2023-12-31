CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-03-24T12:36:34Z creation;2019-03-24T12:36:38Z conversion to V3.1;2019-12-18T07:16:11Z update;2022-11-21T05:29:13Z update;     
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
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  M�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  U�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  W�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ap   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  iL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  kD   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  s    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  u   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  |�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20190324123634  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_168                     2C  DdqNAVIS_A                         0397                            ARGO 011514                     863 @ر���1   @رy\� @<�.H��dq���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111R@w
=@��@��AA=A]A}A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy��C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
D|
=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�1'A�-A�(�A�33A�/A�"�A��A��A�JA���A���A���A�E�A��`A��
A���A���A��A�;dA���A�1'A�?}A�1'A�(�A�bA���A��PA�ffA�I�A�O�A�$�A���A��A�~�A�x�A�l�A�ZA�G�A�1'A�&�A�(�A�{A���A��
A��+A�v�A�ffA�XA�Q�A�K�A�I�A�G�A�E�A�E�A�C�A�?}A�"�A�bA�A��A��A��FA��A�Q�A���A��jA�  A��yA��mA��A�C�A�1'A���A�~�A�I�A��/A��hA�A��jA�bNA���A�n�A��FA���A�t�A���A���A�  A��FA��^A�ZA�t�A���A��A{�;Awl�Au��At�At5?At �Ar�9Aq
=Ao�
Ao�AnffAk"�Aj�+Aj^5Aj1'Ai�Ai��Ai�Ag��Af1'Ad{Acx�Ab9XA`��A`^5A`�A_�^A_`BA]��A[�FA[G�AZ��AZ  AY|�AX��AWt�AU��AS�-AR1ANv�AM�AL1'AKl�AJ�HAJVAHv�AEƨADJAC%AB1'AAXAA�A@bNA>�+A>{A=�#A<��A<~�A;��A;�^A;hsA;&�A:�A:bNA9�
A9�A8�RA8$�A7��A7��A7��A7oA6�RA5��A5x�A5O�A5�A4��A4A�A3�PA2ȴA2�!A2�jA2��A2�RA2�RA2�9A2jA0��A/S�A.��A.��A-�A-O�A+��A+�7A+x�A+`BA+VA)�A)��A(�HA( �A&�A&n�A&JA%�A$�A#�A#�A!��A��A&�A��A��A��A�7Ap�A7LA�/A��A\)A/A��A  A/A�DA�A�RA�^AVA��A9XA1A7LAĜAQ�A�^A��A1'A�PA��A�#A?}A
I�A	�A	33AffA �A��A�;A��AA-AS�A ��A �!A �uA $�@��;@�33@�ff@�7L@�"�@��-@��u@�33@�-@�G�@�Ĝ@�j@��;@�R@���@�o@�$�@�Q�@�ȴ@�-@��@�Q�@�M�@�hs@�z�@�33@��@�I�@�
=@�hs@ܬ@���@���@�1@���@�%@��@���@�Ĝ@Լj@�1'@Ӿw@ӍP@�dZ@ҧ�@��@�Q�@�33@�ff@�@�?}@��H@�O�@�r�@�1'@ǝ�@�E�@ŉ7@��@�"�@��@�p�@�V@�Ĝ@��D@�1'@��
@���@��h@�G�@�&�@�A�@�l�@�+@���@��h@�%@��@�\)@���@�J@��@�A�@�"�@��@�@�^5@�X@��D@�1@�t�@�
=@�n�@�J@���@���@�Q�@�1@��@��y@�v�@�{@��^@��h@�?}@���@�z�@��u@��m@�\)@�V@��^@��@��@�"�@�
=@��@���@���@���@�n�@���@��P@�J@��7@�&�@���@��D@��@��P@�"�@���@��@��^@��-@���@���@���@�x�@�G�@��@��/@�r�@�A�@���@��R@�{@��@��7@�`B@���@���@�l�@�33@��@�n�@�X@�%@��`@��@��@���@��^@�%@���@�Ĝ@���@�r�@��
@��@�S�@��H@��+@�J@��^@�hs@�Ĝ@�I�@��P@�;d@�o@�ȴ@�v�@�M�@�5?@�@���@���@��7@�`B@��@���@�j@��m@�|�@�S�@�+@�^5@�$�@���@���@���@��^@���@��h@��h@��7@�p�@�p�@�&�@��9@�9X@��@��@|�@~�R@}O�@{��@{��@{dZ@{o@z�H@z�@z�@z�\@z�@y�^@x��@w��@w��@w�P@wl�@w�@v�@vff@u��@u�h@u/@t��@t9X@sdZ@r��@r�!@r~�@r^5@r=q@q�#@q��@q��@q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�1'A�-A�(�A�33A�/A�"�A��A��A�JA���A���A���A�E�A��`A��
A���A���A��A�;dA���A�1'A�?}A�1'A�(�A�bA���A��PA�ffA�I�A�O�A�$�A���A��A�~�A�x�A�l�A�ZA�G�A�1'A�&�A�(�A�{A���A��
A��+A�v�A�ffA�XA�Q�A�K�A�I�A�G�A�E�A�E�A�C�A�?}A�"�A�bA�A��A��A��FA��A�Q�A���A��jA�  A��yA��mA��A�C�A�1'A���A�~�A�I�A��/A��hA�A��jA�bNA���A�n�A��FA���A�t�A���A���A�  A��FA��^A�ZA�t�A���A��A{�;Awl�Au��At�At5?At �Ar�9Aq
=Ao�
Ao�AnffAk"�Aj�+Aj^5Aj1'Ai�Ai��Ai�Ag��Af1'Ad{Acx�Ab9XA`��A`^5A`�A_�^A_`BA]��A[�FA[G�AZ��AZ  AY|�AX��AWt�AU��AS�-AR1ANv�AM�AL1'AKl�AJ�HAJVAHv�AEƨADJAC%AB1'AAXAA�A@bNA>�+A>{A=�#A<��A<~�A;��A;�^A;hsA;&�A:�A:bNA9�
A9�A8�RA8$�A7��A7��A7��A7oA6�RA5��A5x�A5O�A5�A4��A4A�A3�PA2ȴA2�!A2�jA2��A2�RA2�RA2�9A2jA0��A/S�A.��A.��A-�A-O�A+��A+�7A+x�A+`BA+VA)�A)��A(�HA( �A&�A&n�A&JA%�A$�A#�A#�A!��A��A&�A��A��A��A�7Ap�A7LA�/A��A\)A/A��A  A/A�DA�A�RA�^AVA��A9XA1A7LAĜAQ�A�^A��A1'A�PA��A�#A?}A
I�A	�A	33AffA �A��A�;A��AA-AS�A ��A �!A �uA $�@��;@�33@�ff@�7L@�"�@��-@��u@�33@�-@�G�@�Ĝ@�j@��;@�R@���@�o@�$�@�Q�@�ȴ@�-@��@�Q�@�M�@�hs@�z�@�33@��@�I�@�
=@�hs@ܬ@���@���@�1@���@�%@��@���@�Ĝ@Լj@�1'@Ӿw@ӍP@�dZ@ҧ�@��@�Q�@�33@�ff@�@�?}@��H@�O�@�r�@�1'@ǝ�@�E�@ŉ7@��@�"�@��@�p�@�V@�Ĝ@��D@�1'@��
@���@��h@�G�@�&�@�A�@�l�@�+@���@��h@�%@��@�\)@���@�J@��@�A�@�"�@��@�@�^5@�X@��D@�1@�t�@�
=@�n�@�J@���@���@�Q�@�1@��@��y@�v�@�{@��^@��h@�?}@���@�z�@��u@��m@�\)@�V@��^@��@��@�"�@�
=@��@���@���@���@�n�@���@��P@�J@��7@�&�@���@��D@��@��P@�"�@���@��@��^@��-@���@���@���@�x�@�G�@��@��/@�r�@�A�@���@��R@�{@��@��7@�`B@���@���@�l�@�33@��@�n�@�X@�%@��`@��@��@���@��^@�%@���@�Ĝ@���@�r�@��
@��@�S�@��H@��+@�J@��^@�hs@�Ĝ@�I�@��P@�;d@�o@�ȴ@�v�@�M�@�5?@�@���@���@��7@�`B@��@���@�j@��m@�|�@�S�@�+@�^5@�$�@���@���@���@��^@���@��h@��h@��7@�p�@�p�@�&�@��9@�9X@��@��@|�@~�R@}O�@{��@{��@{dZ@{o@z�H@z�@z�@z�\@z�@y�^@x��@w��@w��@w�P@wl�@w�@v�@vff@u��@u�h@u/@t��@t9X@sdZ@r��@r�!@r~�@r^5@r=q@q�#@q��@q��@q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111bBBÖBÖBBB��B��B��B�wB�RB�LB�LB�'B�B�B�B��B��B��B��B�RB�wB�wB�wB�jB�FB�B��B��B��B��B��B�bB�oB�{B��B��B�{B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�hB�\B�JB�7B�B}�Bq�BXBG�BE�BD�B@�B7LB5?B,B#�B�BoB
=B��B�fB��B�B��B�hB~�Bq�B\)BC�BB
�sB
�B
�wB
��B
�hB
�B
gmB
E�B
9XB
0!B
+B
(�B
�B
oB
	7B
B	��B	�B	�B	�B	�B	�sB	�fB	�TB	�B	��B	ÖB	�qB	�9B	�B	��B	��B	��B	��B	�{B	�+B	�B	�B	{�B	x�B	r�B	k�B	`BB	S�B	H�B	7LB	0!B	+B	&�B	#�B	�B	�B	%B��B��B��B�B�B�B�sB�sB�mB�B��B��B��B��B�B�B�B�B�B�mB�ZB�NB�HB�HB�HB�;B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��BƨB��B�wB�jB�RB�?B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�hB�7B�%B�B~�B{�B{�Bz�Bz�Bx�Bv�Bu�Bt�Bs�Bq�Bo�Bl�BjBgmBe`BdZBdZBcTBbNBaHB_;B]/B[#BYBW
BVBS�BR�BP�BN�BM�BK�BI�BH�BF�BC�BA�B?}B>wB=qB<jB<jB;dB;dB:^B:^B9XB8RB7LB6FB5?B49B33B33B33B2-B1'B0!B/B/B-B-B-B,B,B+B+B)�B+B+B-B.B-B-B-B-B/B1'B33B49B49B49B49B33B49B49B49B49B49B49B6FB7LB7LB7LB7LB:^B<jB=qB<jB=qB>wB>wB?}BB�BC�BD�BD�BD�BD�BD�BD�BF�BH�BH�BH�BJ�BK�BL�BL�BN�BN�BO�BO�BQ�BR�BT�BXBZBZBZBZB[#B]/B^5B`BBbNBcTBdZBffBiyBjBjBk�Bl�Bm�Bn�Bp�Bq�Br�Bs�Bt�Bu�Bw�Bx�By�Bz�B|�B�B�+B�+B�+B�1B�1B�1B�7B�=B�{B��B��B��B��B��B��B��B��B��B�B�B�B�!B�!B�'B�'B�-B�3B�9B�FB�FB�RB�dB�qB�qB�qB�qB�}BBÖBÖBĜBƨB��B��B��B��B��B�B�#B�;B�BB�BB�BB�HB�fB�sB�yB�B�B�B�B��B��B��B	B	1B		7B	JB	PB	\B	bB	oB	{B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	"�B	(�B	+B	,B	-B	.B	.B	/B	/B	/B	/B	1'B	33B	5?B	7LB	;dB	=qB	=qB	=qB	?}B	C�B	G�B	H�B	H�B	I�B	J�B	I�B	I�B	J�B	L�B	M�B	P�B	T�B	T�B	VB	W
B	XB	YB	ZB	]/B	^5B	_;B	bNB	cTB	ffB	hsB	iyB	iyB	jB	k�B	m�B	s�B	u�B	u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111}B��BÖBÖBªB��B��B��B��B�HB��B��B�B��B�=B�]B�qB�eB�sB�@B��B�B��B��B��B�B��B�cB�8B��B�sB��B�B��B��B��B��B��B��B��B��B��B��B�#B�=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B}Bs�BYBG�BE�BESBA;B7�B6+B,�B$�B�B�B0BB�B��B�qB��B�&B��BtTB`BJ�B�B
�kB
ٴB
�uB
�@B
�,B
��B
k�B
G�B
:�B
0�B
+�B
*�B
!�B
�B

#B
+B
  B	�[B	��B	��B	�B	�*B	�mB	�,B	�B	�B	ĶB	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	|�B	zDB	t�B	m�B	c B	V�B	L~B	9$B	1[B	,"B	'�B	%,B	"hB	�B	1B	 iB�B��B�MB��B�oB�DB�*B��B�B�fB�FB�`B�%B�TB�aB�oB�)B�B�>B��B��B��B�4B�B�vBٚB�eBچB��B��B��B��B�.B��B� B� B�B�4B��B��B�1B�;B�B�qB�XB��B��B�OB��B��B�cB��B�6B�B�_B��B��B��B��B��B�)B��B��B��B��B��B�B|6B|PB{dB{�BzDBwfBvFButBt�Br�Bp�Bm�Bl=Bh�BfLBd�BeBc�BcnBbB`B^5B\]BZQBX+BWsBU2BS�BR BO�BN�BL�BJ�BJ	BIlBF%BB�B@�B?�B>BB<�B<�B<B;�B:�B;B:xB9�B8RB7LB6FB4�B3�B3�B3�B2�B2GB1vB0�B0B.IB./B-�B,�B-CB,qB+�B*�B,B,=B.B/5B./B-�B.B.�B0oB2aB3�B4�B4�B4nB4�B3�B4�B4�B4�B4�B5B5ZB72B8B7�B88B8�B;dB=B=�B="B>]B?B?.B@�BCGBDBD�BEBEBEBE9BE�BGzBIBIBI�BKxBL0BMjBM�BO\BO�BP�BP}BR�BS�BU�BX�BZQBZ�BZ�BZ�B[�B]�B^�B`�Bb�Bc�Bd�BgBi�Bj�BkBlBmBm�Bo Bp�BrBsBtBuBv`BxlBy�BzxB{�B}�B��B�_B�zB�zB�fB�fB��B�#B��B�gB�#B��B�B�B�BB�NB�FB�LB�yB�qB�/B�OB�;B�UB�[B�vB�|B��B��B��B��B�	B��B��B��B��B�B�4B��B��B��B�9B�zB�0B�B�PBϑB��B��BۦBߊB�vB��B��B��B�B��B��B��B�!B�B�B�`B�^B��B	SB	fB		�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	B	B	"B	#:B	#TB	)DB	+6B	,=B	-CB	.IB	.IB	/OB	/OB	/5B	/OB	1[B	3�B	5�B	7�B	;�B	=�B	=�B	=�B	@B	DB	G�B	H�B	H�B	I�B	J�B	I�B	J	B	KB	MB	N<B	Q4B	UB	U2B	V9B	W?B	XEB	YeB	ZQB	]IB	^�B	_pB	b�B	c�B	f�B	h�B	i�B	i�B	j�B	k�B	m�B	s�B	u�B	u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.14(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201904040032012019040400320120190404003201202211182138242022111821382420221118213824201904050020002019040500200020190405002000  JA  ARFMdecpA19c                                                                20190324213633  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190324123634  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190324123636  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190324123637  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190324123637  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190324123637  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190324123637  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190324123637  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20190324123638                      G�O�G�O�G�O�                JA  ARUP                                                                        20190324125534                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190324153042  CV  JULD            G�O�G�O�Fň�                JM  ARCAJMQC2.0                                                                 20190403153201  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190403153201  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190404152000  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114231519                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123824  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                