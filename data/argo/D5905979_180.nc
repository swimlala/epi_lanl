CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-10-12T14:00:57Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20201012140057  20220204114429  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�?�|�1   @�?���@5��x����b�t�j~�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B ffB  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D\��D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy��D���D�` D��D�њD�*=D�R�D��3D��3D�D�UqD��)D�ʏD�D�U�Dګ�D���D�#�D�Z�D�=D�Ӆ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@��@��AA=A]A}A��HA��HA��HA��HA��HA��HA�A��Bp�Bp�Bp�B�
B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_�
Bgp�Bop�Bw
>Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRB��B׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��C�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
D}pD�pDw
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
D�Dw
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
D\�D]w
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
DtФDy��D��{D�[�D���D��D�%�D�ND���D���D��D�P�D���D��D��D�QHDڧ\D��qD�
D�VD��D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��#A��/A��`A��TA��A��A��A��A��A��HAԶFAӰ!A��`A�K�A��A�\)A�
=A��A��
A�AЙ�A�l�A�E�A��A�VA�9XA��A��yA��mA���A�5?A��`A�jA���A�r�A��A�&�A��!A�dZA���A�Q�A�JA��7A��A��RA���A�jA�1'A�n�A�
=A��A�ZA�r�A��A���A���A��FA�(�A���A�G�A�t�A���A��uA��A���A�bA���A���A�oA�dZA�`BA��A���A�^5A��FA��A���A�5?A��`A�bA��A�?}A��A���A��-A�E�A��hA��A�{A�ffA��!A��^A��A�1'A���A��A�\)A�S�A��A�jA��PA�t�A���A��;A�C�A��PA�ȴA�hsA�Q�A�A��A}�Az�AxZAu��Ar�jApVAoS�Am��Ak��AhZAe��Adn�Ab�A_S�A]�7AY�AV�jAS�TAQ�AP~�AMO�AK&�AG��AEK�ACl�AA�A?�FA=�A<�DA:�A9��A933A8(�A7�FA7XA7oA5dZA4JA3��A2^5A0�HA0$�A/O�A-�FA+�;A*�9A*JA(�!A&�/A%�A$�RA#��A"VA!��A!�A v�A�;A��A�yA�FA �AVA$�Ax�A��AM�A��Ax�A~�A��A��AA�!AI�A\)A^5A(�A��A��A�-AA�Ap�A	��A��A�;A�7A�HAn�A��A�A��A?}A�uA��A;dA �jA �@�t�@���@���@�n�@���@��w@�n�@�x�@���@�@�D@�A�@��@�ƨ@�M�@��@�z�@��
@�w@�P@�"�@���@�E�@���@�b@��@�-@�1'@���@�`B@�Z@�`B@�;d@�hs@�(�@ڇ+@ّh@���@�9X@և+@��@ԓu@�Z@� �@��@�Q�@�K�@θR@͡�@�V@�(�@�"�@�-@Ȭ@�"�@�5?@�G�@ēu@��H@�%@�9X@��F@�33@�v�@��@�G�@��@���@���@��w@�~�@�`B@��@�j@��m@���@�C�@�33@�+@��@��7@��u@�Z@�(�@�t�@�C�@�"�@��R@��T@���@�Q�@��@�-@��@��7@�7L@���@�Ĝ@�r�@�A�@���@���@��P@�
=@�~�@�^5@��-@���@�1'@�b@�\)@�C�@��@�
=@�
=@��@�~�@���@�@��@�Q�@��@���@�\)@��@���@��!@���@�33@�o@�=q@��-@��@�j@�z�@���@��`@��@��@�1@��;@���@���@���@��+@�~�@��@��T@���@���@���@��j@���@���@�hs@�Q�@��;@���@��`@��D@�C�@���@�@�@��7@��h@��@�`B@�V@���@��@��@��^@���@��7@�p�@���@�Ĝ@�9X@���@�\)@�C�@�
=@���@���@�"�@�+@�\)@�ȴ@�@�`B@�%@��j@��@�bN@��D@���@�Z@�  @��
@���@�t�@�\)@�t�@�o@���@��!@��\@�-@�J@�{@�n�@�ƨ@���@��j@��@��u@��@�bN@�Z@�9X@��@���@���@���@��+@��y@�@���@�ff@�-@�$�@�J@��#@��h@�&�@���@��/@�Ĝ@�Ĝ@�1@��@�^5@��T@��7@�hs@�p�@�p�@�`B@�V@��j@�Z@���@�C�@���@��@��@��H@��@��@���@��R@�~�@��@���@���@��@�@��@�J@���@�x�@�&�@���@��F@��P@�l�@�+@��H@���@�خ@y`B@q&�@h	�@`��@Xw�@Q�@K/�@C�g@?y�@9S&@0��@*�+@$�|@ ~(@Z@�C@��@��@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A��#A��/A��`A��TA��A��A��A��A��A��HAԶFAӰ!A��`A�K�A��A�\)A�
=A��A��
A�AЙ�A�l�A�E�A��A�VA�9XA��A��yA��mA���A�5?A��`A�jA���A�r�A��A�&�A��!A�dZA���A�Q�A�JA��7A��A��RA���A�jA�1'A�n�A�
=A��A�ZA�r�A��A���A���A��FA�(�A���A�G�A�t�A���A��uA��A���A�bA���A���A�oA�dZA�`BA��A���A�^5A��FA��A���A�5?A��`A�bA��A�?}A��A���A��-A�E�A��hA��A�{A�ffA��!A��^A��A�1'A���A��A�\)A�S�A��A�jA��PA�t�A���A��;A�C�A��PA�ȴA�hsA�Q�A�A��A}�Az�AxZAu��Ar�jApVAoS�Am��Ak��AhZAe��Adn�Ab�A_S�A]�7AY�AV�jAS�TAQ�AP~�AMO�AK&�AG��AEK�ACl�AA�A?�FA=�A<�DA:�A9��A933A8(�A7�FA7XA7oA5dZA4JA3��A2^5A0�HA0$�A/O�A-�FA+�;A*�9A*JA(�!A&�/A%�A$�RA#��A"VA!��A!�A v�A�;A��A�yA�FA �AVA$�Ax�A��AM�A��Ax�A~�A��A��AA�!AI�A\)A^5A(�A��A��A�-AA�Ap�A	��A��A�;A�7A�HAn�A��A�A��A?}A�uA��A;dA �jA �@�t�@���@���@�n�@���@��w@�n�@�x�@���@�@�D@�A�@��@�ƨ@�M�@��@�z�@��
@�w@�P@�"�@���@�E�@���@�b@��@�-@�1'@���@�`B@�Z@�`B@�;d@�hs@�(�@ڇ+@ّh@���@�9X@և+@��@ԓu@�Z@� �@��@�Q�@�K�@θR@͡�@�V@�(�@�"�@�-@Ȭ@�"�@�5?@�G�@ēu@��H@�%@�9X@��F@�33@�v�@��@�G�@��@���@���@��w@�~�@�`B@��@�j@��m@���@�C�@�33@�+@��@��7@��u@�Z@�(�@�t�@�C�@�"�@��R@��T@���@�Q�@��@�-@��@��7@�7L@���@�Ĝ@�r�@�A�@���@���@��P@�
=@�~�@�^5@��-@���@�1'@�b@�\)@�C�@��@�
=@�
=@��@�~�@���@�@��@�Q�@��@���@�\)@��@���@��!@���@�33@�o@�=q@��-@��@�j@�z�@���@��`@��@��@�1@��;@���@���@���@��+@�~�@��@��T@���@���@���@��j@���@���@�hs@�Q�@��;@���@��`@��D@�C�@���@�@�@��7@��h@��@�`B@�V@���@��@��@��^@���@��7@�p�@���@�Ĝ@�9X@���@�\)@�C�@�
=@���@���@�"�@�+@�\)@�ȴ@�@�`B@�%@��j@��@�bN@��D@���@�Z@�  @��
@���@�t�@�\)@�t�@�o@���@��!@��\@�-@�J@�{@�n�@�ƨ@���@��j@��@��u@��@�bN@�Z@�9X@��@���@���@���@��+@��y@�@���@�ff@�-@�$�@�J@��#@��h@�&�@���@��/@�Ĝ@�Ĝ@�1@��@�^5@��T@��7@�hs@�p�@�p�@�`B@�V@��j@�Z@���@�C�@���@��@��@��H@��@��@���@��R@�~�@��@���@���@��@�@��@�J@���@�x�@�&�@���@��F@��P@�l�@�+@��HG�O�@�خ@y`B@q&�@h	�@`��@Xw�@Q�@K/�@C�g@?y�@9S&@0��@*�+@$�|@ ~(@Z@�C@��@��@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB<jB:^B:^B0!B+B'�B%�B"�B"�B#�B"�B!�B!�B �B�B�BB�B49B:^B/B-B33B9XBo�Bq�B�VB��B��B�B�?B�LB��B��B�5B�B��B��B  B	7BDB{B�B�B)�B1'B0!B0!B:^BB�BG�BI�BR�B]/BcTBl�Bl�BhsBaHB\)B\)BT�BP�BG�B2-B$�B$�BoBDB%B��B�;B�BƨB�wB�?B��B��B��B�7Bz�Bp�BcTBZBL�BD�B;dB-B�BuBJB  B
�B
�#B
��B
�RB
��B
�JB
x�B
bNB
O�B
=qB
0!B
uB
B	�B	�/B	ȴB	�qB	�3B	��B	�hB	y�B	m�B	dZB	P�B	A�B	+B	�B	1B��B�B�NB��BƨB�RB�B��B��B��B�uB�\B�DB�=B�1B�+B�B�B�Bz�Bx�Bv�Bq�Bq�Bp�BjBhsBffBffBhsBiyBe`Be`BhsBdZBbNBaHB_;B\)BVBM�BF�B?}B>wBA�B?}B>wB9XB8RB7LB6FB5?B5?B1'B0!B.B.B2-B6FB:^BO�BgmBgmBffBbNBXBP�BR�BVBT�BT�BVBdZBhsBgmBe`BdZBhsBk�Bq�Bu�Bw�Bx�By�Bx�Bv�Bx�Bv�Bw�Bt�Br�Br�Br�Bx�Bv�Bw�B~�B�B�B�%B�7B�7B�=B�hB�oB�oB��B�uB�hB�bB�DB�=B�%B�%B�B�+B�7B�DB�JB�=B�=B�=B�DB�JB�JB�=B�DB�JB�JB�JB�JB�DB�PB�bB�\B�\B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�-B�3B�LB�LB�LB�wBÖBȴB��B��B�B�B�)B�/B�;B�HB�ZB�ZB�`B�sB�B��B��B��B��B��B	B	B	+B	+B	
=B	DB	JB	\B	{B	�B	�B	 �B	!�B	#�B	&�B	(�B	-B	/B	1'B	5?B	;dB	>wB	=qB	=qB	B�B	B�B	C�B	E�B	I�B	M�B	M�B	Q�B	S�B	W
B	YB	YB	ZB	]/B	^5B	`BB	cTB	aHB	bNB	dZB	n�B	n�B	n�B	o�B	p�B	z�B	|�B	|�B	z�B	x�B	z�B	|�B	� B	�B	�B	�B	�%B	�1B	�PB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�3B	�!B	�B	�B	�B	�!B	�'B	�-B	�9B	�9B	�?B	�?B	�?B	�FB	�LB	�RB	�XB	�^B	�jB	�wB	��B	��B	B	ŢB	��B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�)B	�)B	�#B	�B	�5B	�BB	�HB	�TB	�`B	�`B	�`B	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�fB	�`B	�ZB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�TB	�ZB	�ZB	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
B
�B
!|B
,�B
3�B
=qB
@�B
GB
K�B
Q�B
Y�B
_�B
d�B
i�B
m)B
o�B
shB
w�B
|�B
�;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B-B-B-B-B-B-B-B-B-B-B,B)�B)�B�B�B�B�BqBqBwBqBkBkBeBSB5B
�BUB#�B)�B�B�B"�B(�B_:BaFB}�B�DB��B��B��B��B�TBÊB��B�B�WB�oB�B��B��BB
+BCB�B �B�B�B)�B2B75B9ABBxBL�BR�B\B\BW�BP�BK�BK�BD�B@nB78B!�BlBlBB��B��B�wB��BȱB�EB�B��B�yB�aB�%Bx�Bj�B`NBS BI�B<|B4LB+B�BhB,B
�B
�B
�HB
��B
��B
�B
�cB
|B
h�B
RB
?�B
-GB
�B
QB	��B	�B	�B	��B	�ZB	�B	��B	�XB	i�B	]�B	TPB	@�B	1�B	B		�B�5B��BߦB�XB��B��B�cB�.B��B��B��B��BtB{]BzVBxKBwEBu9Bt3Br'Bj�Bh�Bf�Ba�Ba�B`�BZ�BX�BV�BV�BX�BY�BU�BU�BX�BT}BRqBQkBO_BLMBF)B=�B6�B/�B.�B1�B/�B.�B)�B(}B'xB&rB%kB%kB!TB NBBBBB"[B&sB*�B@	BW�BW�BV�BRvBH:BABCBF/BE)BE)BF/BT�BX�BW�BU�BT�BX�B[�Ba�Be�Bg�Bh�BjBh�Bf�Bh�Bf�Bg�Bd�Bb�Bb�Bb�Bh�Bf�Bg�Bo#Bs;BuHBvMBy_By_BzeB��B��B��B��B��B��B��B{nBzgBvPBvPBtDBwVBybB{oB|uBzhBzhBzhB{oB|uB|uBziB{pB|vB|vB|vB|vB{pB}|B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�	B�B�B�"B�B�B�"B�FB�XB�XB�^B�vB�vB�vB��B��B��B��B�B�+B�>B�PB�VB�aB�nBԀBԀBՆBؙBݶB��B��B�
B�B�B�)B�BB�MB�MB�_B�fB�lB�}B	�B	�B	�B	�B	�B	�B	B	B	,B	9B	!EB	%\B	+�B	.�B	-�B	-�B	2�B	2�B	3�B	5�B	9�B	=�B	=�B	BB	DB	G$B	I1B	I1B	J6B	MHB	NNB	P[B	SlB	QaB	RgB	TrB	^�B	^�B	^�B	_�B	`�B	j�B	mB	mB	j�B	h�B	j�B	mB	pB	r!B	s'B	u4B	v:B	xFB	}dB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�$B	�+B	�CB	�CB	�1B	�$B	�$B	�+B	�1B	�7B	�=B	�IB	�IB	�OB	�OB	�OB	�VB	�\B	�aB	�gB	�mB	�yB	��B	��B	��B	��B	��B	��B	�"B	�(B	�(B	�(B	�.B	�.B	�.B	�4B	�4B	�4B	�4B	�.B	�(B	�@B	�MB	�SB	�^B	�jB	�jB	�jB	�pB	�wB	�wB	�}B	كB	كB	كB	كB	�pB	�kB	�eB	�_B	�_B	�_B	�_B	�eB	�eB	�eB	�_B	�eB	�eB	�qB	�xB	�~B	�~B	لB	ܕB	ݛB	�B	ߨB	ޢB	ޢB	ޢB	ߨB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B	�#B
	�B
�B
�B
#�B
-uB
0�B
7B
;�B
A�B
I�B
O�B
T�B
Y�B
]+B
_�B
cjB
g�B
l�B
q<111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.14 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.016(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144292022020411442920220204114429  AO  ARCAADJP                                                                    20201012140057    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20201012140057  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20201012140057  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114429  IP                  G�O�G�O�G�O�                