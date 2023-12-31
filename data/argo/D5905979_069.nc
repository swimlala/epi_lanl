CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-03-18T22:01:16Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        P  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  C   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Jd   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  L8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  S�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  c�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  m    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  tp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  vD   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �$   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �d   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �t   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210318220116  20220204114417  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               EA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ش\B^ގ1   @ش\B^ގ@6cS����d��l�D8   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    EA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2fD2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>�fD?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DP��DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  Dy�=D�HD�g�D���D���D� RD�S�D���D��D�&D�QHD���D��\D��D�\�DڅqD���D�!�D�b�D�HD��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��AA=A]A}A��HA��HA��HA��HA��HA߮A��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��B��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C��C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs��Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��GC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��D w
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
D'}pD'�
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
D1�pD2w
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
D>}pD>�
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
DP�DQw
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
Dy�GD��D�c3D��3D��RD��D�O
D��qD�ؤD�!�D�L�D��HD���D�D�XRDڀ�D��RD�D�^D��D�ڏ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A��A���A��-A���A��+A�l�A�`BA�ZA�Q�A�=qA�/A��A�v�A�{A�=qA��-A�jA�
=A�E�A���A���A�p�A��A��A��A���A�I�A��A�K�A���A��A��\A���A�ffA��A�E�A��DA��wA�|�A���A�l�A���A��A�Q�A�C�A�  A�+A���A�&�A��9A�A�\)A���A��A���A�A�A���A��A��jA�G�A�XA��TA�A�5?A�+A��HA�p�A�&�A��A���A���A�&�A���A�v�A��hA���A�dZA��\A�G�A��A��#A�1A��A��mA�\)A��A�hsA���A�1'A��RA�ZA��yA�A~A�A|�9A{\)Ayl�Av��As��An�/Alr�Ak�wAj�jAj �Ai��Ahz�Ag��Af�Aet�Ac��Ab��Aa��A_��A]"�A[S�AYG�AX1'AUl�AS|�AS"�AR-AOC�ANJAI�^AG��AG"�AF��AE�hACt�AA��A@bA>ĜA=�A=VA;��A9�A9dZA8��A8�A8(�A7��A733A3S�A1�wA1�PA1�A0��A.�HA-l�A,5?A*��A*�DA*~�A*n�A*n�A*ZA)�PA'dZA%33A#ƨA#p�A#33A!�FA 1'A��AA�AO�A1'A��A�wAhsA��A$�A��A"�A%A��A��A/A��A  A�yA��AXA�\A&�Ar�A��A/A
��A
�\A	�PAȴA�mA�HA�-AS�AC�A�!AA�FA�7AO�A+A�A��AI�A&�@��;@��+@�5?@��T@���@�
=@�p�@�"�@�=q@��@�V@�J@��@��@�\@�?}@�P@�h@�?}@�&�@�u@㝲@◍@�=q@�@�7@�hs@�ƨ@�%@�S�@ڟ�@�5?@�`B@���@ӕ�@�~�@Ѳ-@�?}@��@Ь@�bN@��@Ͼw@Ϯ@ύP@�o@·+@��@���@�1'@��
@�|�@ɑh@��@�C�@�M�@� �@�|�@�E�@���@�\)@���@�$�@�r�@�v�@���@���@�1@�|�@��@�ff@��@��#@��h@�`B@�X@�&�@���@��`@���@�l�@�5?@���@�bN@��F@�@�n�@�5?@�{@���@�%@�j@�b@��
@��@�;d@��y@��\@�-@�p�@�%@�Ĝ@���@��D@�r�@�j@�Q�@�9X@�1'@�1@�ƨ@�dZ@�o@�E�@��@��@�r�@� �@��m@�K�@�+@�@��!@�E�@�@��#@��-@�hs@�%@���@��9@�bN@�b@���@���@�t�@�dZ@�K�@��@�$�@��^@���@���@��@���@�dZ@�+@�@���@���@��\@��+@�~�@�n�@�V@�5?@�{@��@�hs@���@���@���@��D@�j@� �@��@��;@�ƨ@�l�@�C�@�"�@��@�&�@���@��@��@�|�@�"�@���@�M�@�J@��T@���@��-@��@�&�@��@�z�@��@�"�@�o@��@�5?@���@��#@��@�-@�E�@�5?@�5?@�$�@�@���@�G�@��D@�Q�@�b@�b@�1@��@���@���@�t�@�\)@�o@��y@��@���@��@�hs@�O�@�?}@���@��j@��@���@��@�bN@�j@��@���@��u@��@�I�@��@���@��F@�`�@x�I@n#:@b�'@Y[W@Q��@L�@EQ�@@u�@9��@5�j@/9�@(�)@#RT@�@�@�@�@e@
�@�Y111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A�A��A���A��-A���A��+A�l�A�`BA�ZA�Q�A�=qA�/A��A�v�A�{A�=qA��-A�jA�
=A�E�A���A���A�p�A��A��A��A���A�I�A��A�K�A���A��A��\A���A�ffA��A�E�A��DA��wA�|�A���A�l�A���A��A�Q�A�C�A�  A�+A���A�&�A��9A�A�\)A���A��A���A�A�A���A��A��jA�G�A�XA��TA�A�5?A�+A��HA�p�A�&�A��A���A���A�&�A���A�v�A��hA���A�dZA��\A�G�A��A��#A�1A��A��mA�\)A��A�hsA���A�1'A��RA�ZA��yA�A~A�A|�9A{\)Ayl�Av��As��An�/Alr�Ak�wAj�jAj �Ai��Ahz�Ag��Af�Aet�Ac��Ab��Aa��A_��A]"�A[S�AYG�AX1'AUl�AS|�AS"�AR-AOC�ANJAI�^AG��AG"�AF��AE�hACt�AA��A@bA>ĜA=�A=VA;��A9�A9dZA8��A8�A8(�A7��A733A3S�A1�wA1�PA1�A0��A.�HA-l�A,5?A*��A*�DA*~�A*n�A*n�A*ZA)�PA'dZA%33A#ƨA#p�A#33A!�FA 1'A��AA�AO�A1'A��A�wAhsA��A$�A��A"�A%A��A��A/A��A  A�yA��AXA�\A&�Ar�A��A/A
��A
�\A	�PAȴA�mA�HA�-AS�AC�A�!AA�FA�7AO�A+A�A��AI�A&�@��;@��+@�5?@��T@���@�
=@�p�@�"�@�=q@��@�V@�J@��@��@�\@�?}@�P@�h@�?}@�&�@�u@㝲@◍@�=q@�@�7@�hs@�ƨ@�%@�S�@ڟ�@�5?@�`B@���@ӕ�@�~�@Ѳ-@�?}@��@Ь@�bN@��@Ͼw@Ϯ@ύP@�o@·+@��@���@�1'@��
@�|�@ɑh@��@�C�@�M�@� �@�|�@�E�@���@�\)@���@�$�@�r�@�v�@���@���@�1@�|�@��@�ff@��@��#@��h@�`B@�X@�&�@���@��`@���@�l�@�5?@���@�bN@��F@�@�n�@�5?@�{@���@�%@�j@�b@��
@��@�;d@��y@��\@�-@�p�@�%@�Ĝ@���@��D@�r�@�j@�Q�@�9X@�1'@�1@�ƨ@�dZ@�o@�E�@��@��@�r�@� �@��m@�K�@�+@�@��!@�E�@�@��#@��-@�hs@�%@���@��9@�bN@�b@���@���@�t�@�dZ@�K�@��@�$�@��^@���@���@��@���@�dZ@�+@�@���@���@��\@��+@�~�@�n�@�V@�5?@�{@��@�hs@���@���@���@��D@�j@� �@��@��;@�ƨ@�l�@�C�@�"�@��@�&�@���@��@��@�|�@�"�@���@�M�@�J@��T@���@��-@��@�&�@��@�z�@��@�"�@�o@��@�5?@���@��#@��@�-@�E�@�5?@�5?@�$�@�@���@�G�@��D@�Q�@�b@�b@�1@��@���@���@�t�@�\)@�o@��y@��@���@��@�hs@�O�@�?}@���@��j@��@���@��@�bN@�j@��@���@��u@��@�I�@��@���@��F@�`�@x�I@n#:@b�'@Y[W@Q��@L�@EQ�@@u�@9��@5�j@/9�@(�)@#RT@�@�@�@�@e@
�@�Y111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB  B
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
��BB{B+B\)Bt�Bq�B�B�BÖB��BBĜB��B�B�HB�mB�sB�yB�B�BB�B"�BuB�B�B�B�BuB�B�B�/B�;B�/B�)B�B�B��B��BƨB�qB�!B��B��B��B�+Br�BbNB[#BW
BK�BK�BK�BE�BF�BI�BL�BM�BG�B=qB=qBB�B.B�B%B
�B
�#B
��B
�3B
��B
��B
}�B
p�B
`BB
S�B
J�B
C�B
=qB
8RB
&�B
�B
�B
	7B
  B	�B	�B	ÖB	�B	��B	��B	��B	��B	�uB	�PB	�+B	� B	o�B	k�B	cTB	\)B	M�B	D�B	<jB	49B	+B	�B	�B	�B	�B		7B��B�B�sB�fB�NB�B��BƨBƨBB�wB�jB�LB�LB�RB�RB�9B�'B�B��B��B��B�{B�uB�VB�DB�1B�B�B�+B�+B�+B�%B�%B�B|�Bt�Bq�Bo�Bk�BiyBffBe`BbNB`BB`BB]/B[#BZBZBZB[#B\)B[#B[#BYBW
BXBVBT�BS�BQ�BS�BQ�BQ�BQ�BQ�BO�BN�BJ�BI�BJ�BJ�BI�BJ�BL�BQ�BR�BT�BW
BXBYBYBYBZB[#B[#B[#B[#BZBZBZB[#BZBW
BXB\)B_;B`BBbNBdZBe`BgmBiyBhsBhsBhsBiyBiyBiyBiyBhsBgmBiyBk�Bm�Bo�Bo�Bp�Bq�Bt�Bt�Bu�Bv�Bw�Bx�By�By�By�By�By�By�By�Bx�Bw�Bx�Bx�Bw�B{�B}�B� B�B�PB�bB�uB��B��B��B��B��B�B�B�-B�?B�RB�^B�qB�wB�}B��B��B��BBÖBÖBĜBɺB��B��B��B�
B�B�)B�/B�5B�;B�TB�mB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	  B	B	B	B	
=B	DB	hB	�B	�B	�B	�B	�B	 �B	"�B	%�B	'�B	(�B	)�B	,B	.B	/B	0!B	2-B	5?B	6FB	8RB	9XB	9XB	:^B	=qB	A�B	D�B	K�B	Q�B	R�B	S�B	T�B	W
B	XB	[#B	\)B	]/B	]/B	]/B	^5B	_;B	`BB	`BB	aHB	ffB	iyB	k�B	l�B	l�B	o�B	s�B	u�B	u�B	y�B	}�B	~�B	~�B	�B	�+B	�+B	�+B	�1B	�7B	�DB	�PB	�VB	�hB	�hB	�hB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�9B	�?B	�LB	�^B	�qB	�}B	�}B	�}B	��B	ÖB	ŢB	ƨB	ƨB	ǮB	ɺB	ɺB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B
MB
�B
�B
(�B
/�B
4nB
;B
@4B
EmB
J�B
Q�B
X�B
]�B
b�B
d�B
iyB
l�B
t�B
w�B
~111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�8B
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
�,B
�JB�B#8BT\Bl�Bi�B}PB�JB��B��B��B��B� B�=B�tBߙB��B�B�B��B�1B�B�B�B��B��B�B�B�B��B�FB�^B�jB�^B�XB�MB�4B�"B�
B��B��B�TB�#B��B��BaBj�BZ�BS\BOCBDBDBDB=�B>�BA�BEBFB?�B5�B5�B:�B&QB�B
�eB
��B
�fB
��B
�yB
�$B
��B
v>B
h�B
X�B
LEB
CB
;�B
5�B
0�B
9B
�B
�B
�B	�SB	��B	�sB	��B	�mB	�IB	�%B	�B	� B	��B	��B	�B	x\B	g�B	c�B	[�B	T�B	F3B	<�B	4�B	,�B	#eB	B	B	B	�B	�B�HB��B��B��BڷB�zB�>B�B�B��B��B��B��B��B��B��B��B��B�|B�FB��B��B��B��B��B��B��B}�B}�B�B�B�B~�B~�B}�BuaBm/BjBhBc�Ba�B^�B]�BZ�BX�BX�BU�BS�BR�BR�BR�BS�BT�BS�BS�BQ�BO�BP�BN{BMuBLpBJdBLpBJdBJdBJdBJdBHWBGQBC:BB3BC:BC:BB3BC:BEFBJeBKkBMwBO�BP�BQ�BQ�BQ�BR�BS�BS�BS�BS�BR�BR�BR�BS�BR�BO�BP�BT�BW�BX�BZ�B\�B]�B_�Ba�B`�B`�B`�Ba�Ba�Ba�Ba�B`�B_�Ba�Bc�BfBhBhBiBj$Bm6Bm7Bn>BoCBpIBqOBrUBrUBrUBrUBrUBrUBrUBqPBpJBqPBqPBpJBtbBvoBx{Bz�B��B��B��B�B�+B�+B�1B�VB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�2B�KB�cB�oBρBҔBԠBզB֬BײB��B��B��B��B�B�B�B� B�,B�EB�QB�\B�cB�cB�iB�iB�iB�oB�oB�uB�{B��B��B	�B	�B		�B	�B	B	B	,B	2B	9B	EB	VB	 cB	!iB	"oB	${B	&�B	'�B	(�B	*�B	-�B	.�B	0�B	1�B	1�B	2�B	5�B	9�B	=B	D9B	J]B	KcB	LiB	MoB	O{B	P�B	S�B	T�B	U�B	U�B	U�B	V�B	W�B	X�B	X�B	Y�B	^�B	a�B	c�B	d�B	d�B	hB	l&B	n3B	n3B	rJB	vcB	wiB	wiB	|�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�&B	�,B	�3B	�?B	�]B	�iB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�&B	�&B	� B	�-B	�3B	�3B	�3B	�3B	�3B	�3B	�>B	�DB	�DB	�JB	�PB	�cB	�iB	�iB	�oB	тB	҈B	тB	�{B	҈B	҈B	��B	��B
.B
�B
 �B
(TB
,�B
3�B
8�B
=�B
CBB
I�B
QIB
VB
[7B
]B
a�B
e&B
m"B
p5B
vs111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.14 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144172022020411441720220204114417  AO  ARCAADJP                                                                    20210318220116    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210318220116  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210318220116  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114417  IP                  G�O�G�O�G�O�                