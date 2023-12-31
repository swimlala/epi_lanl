CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:14:06Z creation      
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141406  20220204114421  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               lA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��5I@�1   @����ۦ@5z�1'�c&ȴ9X1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    lA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBpffBx  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDyeD�{D�UqD���D���D��RD�\)D���D�θD� RD�\)D���D���D�{D�X Dڍ�D�ɚD�D�` D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��AA=A]A}A��HA��HA��HA��HAϮA��HA��HA��Bp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_�
Bg�
Bo�
Bwp�Bp�B��RB��RB��RB��RB��B��B��B��RB��RB��RB��B��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB��B�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CACC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm��Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
Dp�D�Dw
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
D}pD�
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
D`�Daw
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
DtФDy\)D� D�P�D��RD��qD���D�W�D��{D��=D��D�W�D��D��qD� D�S�DډHD��D��D�[�D�
D��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��#A��/A��/A��/A��;A��;A��HA��HA��HA��TA��TA��`A��A��yA��A��A��A��A�C�A�JA�\)A܁A܋DA�|�A�M�A�/A�9XA�I�A�"�A�K�A�VAٕ�A��A�5?A��A���A���A�|�A�bNAǃA�Q�AċDA�z�A��A�K�A�O�A���A�5?A��wA��A�9XA���A�l�A���A�JA�-A�^5A��^A�%A��#A��PA��HA���A�+A�z�A�O�A�A��/A�1A��9A�M�A���A�I�A��A�33A��A�\)A���A���A�JA�/A�=qA��A���A�C�A�hsA�=qA���A�A�A�ZA�x�A���A��hA�ȴA��A�t�A��
A��\A��A��A���A��A��#A��`A��A���A� �A�dZA�=qA~��A|{Ax$�Ar�Ao��Am��AkG�Ah��Af��Ad�+Ab{A]�-A[�TAYAW��AU��AS�AR5?AQhsAP�ANĜAKO�AI%AG��AFbAEVAC;dAA�AAl�A@$�A=��A<n�A;�PA:=qA9A7\)A5�A4��A4I�A3�A2Q�A0��A/hsA.��A-A-A+�FA*��A)
=A'�#A&�uA%�wA$ĜA$1A#oA"I�A!�A!A��A33A �A�RAZA{At�A^5A�^A"�AM�AK�A��A~�A;dAI�A�Al�A�A�uA\)A�A�9AbAdZA{A%A
bA	��A	C�A��A��A$�A�A%A��A$�A+A��A  AXA��A{AK�A @���@�t�@�33@��y@�?}@���@�~�@�$�@���@�r�@��R@��9@�&�@��`@�5?@��/@��;@���@홚@�V@�1'@�@�l�@ꟾ@���@��@��@�Z@�@��y@�-@�x�@��@�^5@�ff@�\@��@��@�@�=q@�@�ȴ@�O�@�ƨ@Դ9@��@�b@�v�@��#@Ͳ-@�X@���@�b@�z�@̬@�A�@��y@�z�@�p�@ŉ7@���@Å@�l�@�r�@��;@�1@�v�@�  @��@�E�@��@�7L@�V@�A�@�1'@�9X@�\)@��@�p�@��9@��-@��@�Q�@��@�~�@�E�@��@�I�@�ȴ@�E�@���@��D@�ƨ@�dZ@���@�~�@�v�@���@�?}@�Ĝ@�z�@��w@�"�@��\@�=q@��@��^@�7L@�I�@��m@�|�@�+@��+@���@���@�Ĝ@�z�@�b@��F@��@��@���@��+@�{@��@�hs@�G�@��@��`@�z�@�Z@���@�t�@�dZ@��@��y@��@�ȴ@���@�=q@���@���@�bN@�A�@� �@�(�@�1'@�r�@�t�@�33@��@���@��+@��\@�v�@�E�@�E�@�ff@���@���@�ȴ@��\@���@�-@���@�hs@��h@�@�x�@��@��@��@�r�@�bN@�1'@�(�@�(�@�A�@�b@���@���@�t�@�;d@��@�ȴ@���@�n�@�~�@���@���@���@��R@�n�@�V@�=q@�@��-@��@�`B@�7L@�&�@�V@���@���@��/@�Ĝ@��9@���@���@��u@�I�@�1'@�b@��w@�33@���@���@�ȴ@��!@��!@��!@���@�M�@��T@��^@���@�7L@�V@���@���@���@���@�I�@�b@��F@�dZ@�o@��y@���@���@�~�@�ff@�=q@��T@�X@�V@��@��/@���@��9@��D@�r�@�b@��F@���@�l�@�33@��@�@���@��+@�=q@��#@�@���@�O�@�V@���@�j@�Q�@�1@��@l�@;d@~ȴ@~V@|�@v�2@o9�@gy�@a^�@[��@T|�@J�]@C��@;��@6�@1?}@+x@%��@!�#@!�@�Y@xl@0U@	��@�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A��#A��/A��/A��/A��;A��;A��HA��HA��HA��TA��TA��`A��A��yA��A��A��A��A�C�A�JA�\)A܁A܋DA�|�A�M�A�/A�9XA�I�A�"�A�K�A�VAٕ�A��A�5?A��A���A���A�|�A�bNAǃA�Q�AċDA�z�A��A�K�A�O�A���A�5?A��wA��A�9XA���A�l�A���A�JA�-A�^5A��^A�%A��#A��PA��HA���A�+A�z�A�O�A�A��/A�1A��9A�M�A���A�I�A��A�33A��A�\)A���A���A�JA�/A�=qA��A���A�C�A�hsA�=qA���A�A�A�ZA�x�A���A��hA�ȴA��A�t�A��
A��\A��A��A���A��A��#A��`A��A���A� �A�dZA�=qA~��A|{Ax$�Ar�Ao��Am��AkG�Ah��Af��Ad�+Ab{A]�-A[�TAYAW��AU��AS�AR5?AQhsAP�ANĜAKO�AI%AG��AFbAEVAC;dAA�AAl�A@$�A=��A<n�A;�PA:=qA9A7\)A5�A4��A4I�A3�A2Q�A0��A/hsA.��A-A-A+�FA*��A)
=A'�#A&�uA%�wA$ĜA$1A#oA"I�A!�A!A��A33A �A�RAZA{At�A^5A�^A"�AM�AK�A��A~�A;dAI�A�Al�A�A�uA\)A�A�9AbAdZA{A%A
bA	��A	C�A��A��A$�A�A%A��A$�A+A��A  AXA��A{AK�A @���@�t�@�33@��y@�?}@���@�~�@�$�@���@�r�@��R@��9@�&�@��`@�5?@��/@��;@���@홚@�V@�1'@�@�l�@ꟾ@���@��@��@�Z@�@��y@�-@�x�@��@�^5@�ff@�\@��@��@�@�=q@�@�ȴ@�O�@�ƨ@Դ9@��@�b@�v�@��#@Ͳ-@�X@���@�b@�z�@̬@�A�@��y@�z�@�p�@ŉ7@���@Å@�l�@�r�@��;@�1@�v�@�  @��@�E�@��@�7L@�V@�A�@�1'@�9X@�\)@��@�p�@��9@��-@��@�Q�@��@�~�@�E�@��@�I�@�ȴ@�E�@���@��D@�ƨ@�dZ@���@�~�@�v�@���@�?}@�Ĝ@�z�@��w@�"�@��\@�=q@��@��^@�7L@�I�@��m@�|�@�+@��+@���@���@�Ĝ@�z�@�b@��F@��@��@���@��+@�{@��@�hs@�G�@��@��`@�z�@�Z@���@�t�@�dZ@��@��y@��@�ȴ@���@�=q@���@���@�bN@�A�@� �@�(�@�1'@�r�@�t�@�33@��@���@��+@��\@�v�@�E�@�E�@�ff@���@���@�ȴ@��\@���@�-@���@�hs@��h@�@�x�@��@��@��@�r�@�bN@�1'@�(�@�(�@�A�@�b@���@���@�t�@�;d@��@�ȴ@���@�n�@�~�@���@���@���@��R@�n�@�V@�=q@�@��-@��@�`B@�7L@�&�@�V@���@���@��/@�Ĝ@��9@���@���@��u@�I�@�1'@�b@��w@�33@���@���@�ȴ@��!@��!@��!@���@�M�@��T@��^@���@�7L@�V@���@���@���@���@�I�@�b@��F@�dZ@�o@��y@���@���@�~�@�ff@�=q@��T@�X@�V@��@��/@���@��9@��D@�r�@�b@��F@���@�l�@�33@��@�@���@��+@�=q@��#@�@���@�O�@�V@���@�j@�Q�@�1@��@l�@;d@~ȴG�O�@|�@v�2@o9�@gy�@a^�@[��@T|�@J�]@C��@;��@6�@1?}@+x@%��@!�#@!�@�Y@xl@0U@	��@�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
P�B
O�B
O�B
R�B
Q�B
P�B
n�B
�9B=qBT�B\)BdZBdZBk�Bs�B� B�Bw�B}�Bn�BgmBffBl�BhsBz�B��B�qB��B�HB�BJB.BP�BP�BM�BPB�-B��B��B��B��B�-B�jB�XBȴB��B�
BƨB��B��Bn�BD�B9XB[#Bx�B�B��BƨB�yBuB$�B.B$�B�BuBhB�BJB��B�B�)B��B�^B�'B��B��B�B[#B;dB5?B1'B+B�BhB
=B
��B
�5B
��B
�dB
��B
��B
�7B
v�B
bNB
S�B
E�B
A�B
7LB
$�B
B	�#B	��B	�B	��B	�oB	�B	m�B	S�B	+B	�B	\B	B��B��B��B�B�B�`B�BɺBƨB�wB�9B�B��B��B��B��B��B��B��B��B�uB�{B�hB�\B�bB�PB�PB�VB�PB�DB�DB�7B�+B�%B�B�B�B~�B~�B}�B|�Bz�B{�By�Bz�By�Bw�Bv�Bu�Bw�Bw�Bt�Bt�Bs�Bs�By�B�Bz�By�Bv�Bw�Bv�Bv�B|�B{�B|�B{�B{�B{�B~�By�B{�B�B�B� B� B� B}�B}�B|�B{�B{�Bz�Bz�Bx�Bv�Bt�Bu�Bt�Bt�Bu�Bz�B{�B|�B~�B�B�B�+B�1B�B�=B�JB�=B�%B�B�B�B�B�B�%B�+B�=B�hB�oB�\B�\B�\B�hB��B��B��B�B�B�3B�qB��B�}B�wB�^B�?B�!B�B��B��B�bB�JB�=B�DB�DB�JB�\B��B��B��B��B��B�uB�uB��B�bB�oB��B��B��B��B�oB�{B�{B�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�9B�9B�?B�?B�FB�RB�^B�dB�wB��BÖBĜBƨBɺB��B��B��B��B��B�B�B�)B�/B�HB�fB�B�B�B�B�B��B��B��B	  B	B	B	B	%B	+B		7B	PB	oB	oB	{B	�B	�B	�B	�B	�B	�B	 �B	�B	 �B	!�B	"�B	#�B	%�B	'�B	-B	-B	0!B	33B	7LB	<jB	=qB	@�B	B�B	D�B	G�B	L�B	O�B	S�B	W
B	W
B	YB	YB	ZB	]/B	aHB	e`B	e`B	k�B	p�B	q�B	r�B	u�B	u�B	v�B	y�B	{�B	z�B	z�B	{�B	|�B	� B	�B	�B	�+B	�=B	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�9B	�XB	�^B	�dB	�dB	�dB	�dB	�dB	�dB	�jB	�dB	�jB	�wB	�}B	��B	B	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�)B	�/B	�/B	�/B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�HB	�HB	�HB	�NB	�TB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	��B	�}B
xB
+B
"hB
*�B
5�B
="B
C-B
J	B
M�B
TB
YeB
\�B
a�B
i_B
l�B
r�B
v�B
{B
|j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
F�B
E�B
E�B
H�B
G�B
F�B
dOB
��B3BJ�BQ�BY�BY�Ba(BiXBu�Bw�BmqBs�Bd;B]B\Bb0B^Bp�B�(B�B�lB��B�AB�B#�BF{BF|BCjB�B��B�MB�MB��B��B��B�B��B�YB�}B̮B�MB�/B��BdEB:MB/
BP�Bn�Bx�B�KB�OB�B	B}B#�B~BYB	BB$B�B��B�JB��B�-B�	B��B�kB�/Bx�BP�B1B*�B&�B �BMB"B
��B
�B
��B
��B
�&B
��B
�YB
~�B
l�B
XB
I�B
;oB
7WB
-B
�B	��B	��B	�]B	��B	��B	�MB	z�B	crB	I�B	 �B	�B	FB��B��B��B��B�B�sB�OB�B��B��B�jB�-B�B��B��B��B��B��B��B�B�yB�mB�tB�aB�UB�[B�JB�JB�PB�JB�>B�?B2B}&B| BzBwBwBt�Bt�Bs�Br�Bp�Bq�Bo�Bp�Bo�Bm�Bl�Bk�Bm�Bm�Bj�Bj�Bi�Bi�Bo�BwBp�Bo�Bl�Bm�Bl�Bl�Br�Bq�Br�Bq�Bq�Bq�Bt�Bo�Bq�BwBwBvBvBvBs�Bs�Br�Bq�Bq�Bp�Bp�Bn�Bl�Bj�Bk�Bj�Bj�Bk�Bp�Bq�Br�Bt�Bw	B{!B}-B~3ByB�?B�LB�@B|(B{"BzByBzB{"B|(B}.B�@B�kB�rB�_B�_B�_B�kB��B��B��B�	B�B�4B�qB��B�}B�wB�_B�@B�#B�B��B��B�gB�OB�CB�IB�IB�OB�aB��B��B��B��B��B�{B�{B��B�hB�uB��B��B��B��B�uB��B��B�|B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�!B�>B�>B�DB�DB�KB�WB�cB�iB�|B��B��B��B��B��B��B��B��B��B��B�B� B�,B�2B�KB�iB�B�B�B�B�B��B��B��B�B�B�B� B�&B�+B�7B	PB	oB	oB	
{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#B	#B	&B	)1B	-IB	2gB	3nB	6�B	8�B	:�B	=�B	B�B	E�B	I�B	MB	MB	OB	OB	PB	S*B	WBB	[ZB	[ZB	a~B	f�B	g�B	h�B	k�B	k�B	l�B	o�B	q�B	p�B	p�B	q�B	r�B	u�B	xB	zB	}#B	�4B	�SB	�_B	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�LB	�RB	�XB	�XB	�XB	�XB	�XB	�XB	�^B	�XB	�^B	�kB	�qB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�!B	�!B	�'B	�-B	�-B	�-B	�4B	�:B	�:B	�:B	�:B	�@B	�FB	�KB	�KB	�QB	�QB	�QB	�WB	�WB	�dB	�pB	�pB	�pB	�pB	�vG�O�B	��B	�mB
gB
B
VB
 �B
+�B
3B
9B
?�B
C�B
I�B
OQB
R�B
W�B
_JB
b�B
h�B
lB
qB
rU111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.14 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.01(+/-0.004) in PSS-78.                                                                                                                                                                                        Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144212022020411442120220204114421  AO  ARCAADJP                                                                    20200618141406    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141406  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141406  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114421  IP                  G�O�G�O�G�O�                