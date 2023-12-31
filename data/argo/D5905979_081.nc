CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:13Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170913  20220204114419  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               QA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��]�C�1   @��^d��@7XbM��c�Ƨ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    QA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D(��D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/�fD0  D0� D1fD1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DWfDW� DX  DX� DYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�fD�+3D�`�D��
D��\D��D�S3D��fD�ÅD�(RD�Y�D�� D���D�!HD�b�Dڜ)D��HD��D�eqD�D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@��@��AA=A]A}A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�B�
B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB��B��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
D�Dw
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
D}pD�
Dw
D�pDw
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
D(�D)w
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
D/}pD/�
D0w
D0�pD1w
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
DV�pDWw
DW�
DXw
DX�pDYw
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
Dt��Dy�pD�&�D�\{D���D���D�3D�N�D���D��
D�#�D�UD���D��qD��D�^Dڗ�D���D�RD�`�D�
D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��
A��#A��#A��/A��#A��;A��/A��;A��;A��/AͲ-A͝�A�-Aɟ�A�JA�^5Ağ�A���A�ffA��^A�~�A�~�A�?}A�"�A�"�A�{A�Q�A���A��A���A��jA��hA�1A��RA�A�A���A���A�M�A���A���A�XA��
A�v�A�\)A�oA���A�%A���A�x�A�\)A�oA���A��#A��hA�O�A�  A���A�hsA�?}A�ȴA�=qA���A�z�A�l�A�bNA�K�A�"�A�ĜA�~�A�(�A��DA��mA�jA�=qA�VA�z�A�33A���A���A�ZA�jA�`BA�1'A�XA���A��uA�hsA��A�"�A�=qA�JA���A� �A���A�G�A�/A�/A�C�A��A�oA���A�A��yA�oA���A��A��+A�5?A�{A���A�7LA��A���A���A�ZA���A�&�A�&�A�t�AG�A}x�A{O�Az��AzA�AyS�Avv�As�wAr��ArQ�Aq33Akx�AiAh9XAg�Af��Af-Ad�`Ab�A_�A\z�A[K�AY;dAXA�AW�;AU�ASt�AR��AP��AO\)AL��AJ�\AIdZAH��AG�PAF�!AE�PAD�\ABffAAoA?�-A>{A=K�A:�A8�A6��A6JA5�hA5t�A5dZA4��A4(�A3dZA2�`A1�hA0jA/�PA.ȴA-�mA,��A+l�A*5?A'�wA&�A%/A$�+A$bA"�`A ĜA(�AVA��A��A
=AbNA��A�RA  A�A�-An�A1A��A��AC�A^5A�-A&�A1'A/A��A�7A+A
�A
��A
��A
A�HA��A��Al�A�At�A\)A��A~�AA �@�5?@��@�ƨ@�t�@��!@���@���@��@���@�o@��+@�v�@�1@�u@�V@�-@�j@�!@��T@��@�X@�p�@��m@�7@�\)@ާ�@���@ݺ^@��@۶F@�$�@�O�@�V@؃@պ^@��m@��@�J@�@�?}@�I�@͙�@��`@�ȴ@ȓu@�@ě�@�1'@�b@�  @���@��;@���@�t�@�33@�~�@�`B@�Q�@���@��@���@���@�z�@�A�@��@��@�+@���@�@���@�r�@���@���@�dZ@�@��+@���@��H@�^5@�x�@�/@�Ĝ@��!@��@�j@���@��@�S�@���@�E�@�{@�J@��^@���@��/@�j@���@�33@��+@���@��j@�r�@���@�
=@�S�@�@���@�^5@�{@�
=@���@�\)@�dZ@��;@�Z@�"�@��R@�X@�9X@��/@���@��R@�E�@���@�hs@���@��9@�bN@���@�
=@��H@���@��^@��@��@���@�(�@��
@��@�5?@�@�=q@�`B@��@�j@�9X@��@���@���@���@�
=@��!@��+@�ff@�-@�-@�-@�-@�-@�$�@�J@���@���@�x�@�G�@��@���@���@�j@�  @��m@��@�o@��R@��+@�^5@���@���@��7@�G�@��@�5?@���@�@���@�J@��@�C�@�\)@�ȴ@��#@��@�@���@��P@�33@���@��H@���@���@�~�@��\@�-@���@���@�1'@��F@�t�@�l�@�C�@�+@�@�ȴ@��\@�n�@�$�@��T@��-@��@�hs@�G�@��7@��@�&�@��j@��9@��/@��@���@��D@� �@��w@��@�|�@�\)@�C�@�;d@�+@���@�V@��@���@�x�@�G�@���@��u@��D@�r�@�Q�@� �@���@���@��w@���@�dZ@�+@��y@��y@��y@��H@��@���@�e,@}|@so�@k��@cs@\�p@X��@R�r@IVm@B�b@:�2@5��@0-�@)A @$9X@�6@_�@Z@��@�@Mj111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��
A��#A��#A��/A��#A��;A��/A��;A��;A��/AͲ-A͝�A�-Aɟ�A�JA�^5Ağ�A���A�ffA��^A�~�A�~�A�?}A�"�A�"�A�{A�Q�A���A��A���A��jA��hA�1A��RA�A�A���A���A�M�A���A���A�XA��
A�v�A�\)A�oA���A�%A���A�x�A�\)A�oA���A��#A��hA�O�A�  A���A�hsA�?}A�ȴA�=qA���A�z�A�l�A�bNA�K�A�"�A�ĜA�~�A�(�A��DA��mA�jA�=qA�VA�z�A�33A���A���A�ZA�jA�`BA�1'A�XA���A��uA�hsA��A�"�A�=qA�JA���A� �A���A�G�A�/A�/A�C�A��A�oA���A�A��yA�oA���A��A��+A�5?A�{A���A�7LA��A���A���A�ZA���A�&�A�&�A�t�AG�A}x�A{O�Az��AzA�AyS�Avv�As�wAr��ArQ�Aq33Akx�AiAh9XAg�Af��Af-Ad�`Ab�A_�A\z�A[K�AY;dAXA�AW�;AU�ASt�AR��AP��AO\)AL��AJ�\AIdZAH��AG�PAF�!AE�PAD�\ABffAAoA?�-A>{A=K�A:�A8�A6��A6JA5�hA5t�A5dZA4��A4(�A3dZA2�`A1�hA0jA/�PA.ȴA-�mA,��A+l�A*5?A'�wA&�A%/A$�+A$bA"�`A ĜA(�AVA��A��A
=AbNA��A�RA  A�A�-An�A1A��A��AC�A^5A�-A&�A1'A/A��A�7A+A
�A
��A
��A
A�HA��A��Al�A�At�A\)A��A~�AA �@�5?@��@�ƨ@�t�@��!@���@���@��@���@�o@��+@�v�@�1@�u@�V@�-@�j@�!@��T@��@�X@�p�@��m@�7@�\)@ާ�@���@ݺ^@��@۶F@�$�@�O�@�V@؃@պ^@��m@��@�J@�@�?}@�I�@͙�@��`@�ȴ@ȓu@�@ě�@�1'@�b@�  @���@��;@���@�t�@�33@�~�@�`B@�Q�@���@��@���@���@�z�@�A�@��@��@�+@���@�@���@�r�@���@���@�dZ@�@��+@���@��H@�^5@�x�@�/@�Ĝ@��!@��@�j@���@��@�S�@���@�E�@�{@�J@��^@���@��/@�j@���@�33@��+@���@��j@�r�@���@�
=@�S�@�@���@�^5@�{@�
=@���@�\)@�dZ@��;@�Z@�"�@��R@�X@�9X@��/@���@��R@�E�@���@�hs@���@��9@�bN@���@�
=@��H@���@��^@��@��@���@�(�@��
@��@�5?@�@�=q@�`B@��@�j@�9X@��@���@���@���@�
=@��!@��+@�ff@�-@�-@�-@�-@�-@�$�@�J@���@���@�x�@�G�@��@���@���@�j@�  @��m@��@�o@��R@��+@�^5@���@���@��7@�G�@��@�5?@���@�@���@�J@��@�C�@�\)@�ȴ@��#@��@�@���@��P@�33@���@��H@���@���@�~�@��\@�-@���@���@�1'@��F@�t�@�l�@�C�@�+@�@�ȴ@��\@�n�@�$�@��T@��-@��@�hs@�G�@��7@��@�&�@��j@��9@��/@��@���@��D@� �@��w@��@�|�@�\)@�C�@�;d@�+@���@�V@��@���@�x�@�G�@���@��u@��D@�r�@�Q�@� �@���@���@��w@���@�dZ@�+@��y@��y@��y@��H@��G�O�@�e,@}|@so�@k��@cs@\�p@X��@R�r@IVm@B�b@:�2@5��@0-�@)A @$9X@�6@_�@Z@��@�@Mj111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
u�B
v�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
u�B
r�B
p�B
jB
s�B
bNB
T�B
;dB
33B
Q�B
ZB
z�B
�B
�wB
��B
��B
�ZBPB1BPBPBbB�B:^B�DB��B�B�LB�wB��B�B�B�B1BPBbB�B�B�B'�B1'B;dB<jB?}BE�BG�BM�BR�BS�BS�B[#B`BBgmBk�Bl�Bn�Bq�Bt�Bz�B}�B� B�B�JB�JB�=B�=B�JB�PB�PB�PB�JB�hB�\B�DB�B}�By�Bu�BjB[#BW
BR�BE�B7LB+B�B��B�ZB�#BŢB�qB�3B�B��B��Bz�Bm�BgmB`BBP�B �B
�B
��B
�FB
��B
��B
�B
v�B
k�B
O�B
D�B
8RB
'�B
!�B
�B
�B
JB	��B	�B	�mB	�NB	ĜB	�B	��B	��B	��B	��B	�{B	�B	p�B	_;B	Q�B	F�B	>wB	<jB	33B	'�B	!�B	�B	\B	B��B�B�B�B�B�B�B�yB�ZB�)B�
B��B��B�wB�LB�-B�!B�B�B�B��B��B��B��B��B��B��B�hB�JB�B|�By�Bv�Bs�Bo�BjBiyBe`B^5B\)B\)BaHB_;B_;B]/B[#BZBYBZBXBVBT�BW
BVBVBT�BT�BS�BS�BT�BS�BQ�BP�BO�BN�BO�BP�BYBT�BT�BP�BM�BK�BJ�BE�BA�B>wB<jB7LB7LB9XB;dB;dB;dB:^B:^B:^B;dB=qB=qB<jB<jB9XB7LB7LB=qBD�BS�B\)B_;B\)BXB]/B`BBcTBcTBbNBgmBp�Bq�Bw�By�Bq�Bn�Bn�Bt�Bq�Bp�Bm�Bl�Bn�BhsBhsBdZBe`Be`Be`Be`Be`BgmBn�Bw�B{�B~�B~�B~�Bz�Bz�By�By�Bz�Bz�Bz�Bz�Bz�B{�B|�B�B�B�+B�=B�PB�bB��B��B��B��B��B��B��B��B��B��B��B��B�!B�-B�LB�^B�dB�jB�wB��BBĜBȴB��B��B��B�B�#B�`B�B��B	B	%B	oB	�B	�B	�B	�B	%�B	#�B	"�B	"�B	�B	$�B	+B	5?B	8RB	9XB	:^B	:^B	;dB	:^B	:^B	=qB	?}B	A�B	F�B	H�B	L�B	O�B	Q�B	W
B	YB	XB	VB	ZB	_;B	`BB	_;B	_;B	]/B	ZB	XB	W
B	XB	]/B	^5B	_;B	aHB	cTB	dZB	dZB	dZB	e`B	ffB	gmB	k�B	m�B	n�B	p�B	r�B	t�B	u�B	v�B	w�B	y�B	{�B	}�B	~�B	� B	�B	�B	�B	�B	�VB	��B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�!B	�'B	�!B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�-B	�9B	�FB	�FB	�LB	�XB	�^B	�XB	�^B	�dB	�qB	�wB	�wB	�wB	�wB	��B	��B	ÖB	ĜB	ĜB	ĜB	ŢB	ŢB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�FB	�vB
B
�B
�B
!�B
%�B
-wB
8B
=�B
E�B
I�B
O�B
U�B
ZkB
_!B
d@B
lB
p�B
u�B
y$111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
nBB
oHB
nBB
oHB
oHB
oHB
oHB
oHB
oHB
nBB
k/B
i#B
b�B
l6B
Z�B
M�B
3�B
+�B
JoB
R�B
saB
��B
��B
�CB
�[B
��B�B �B�B�B�BB2�B��B�<B��B��B��B�AB��B��B�B �B�B�B
BBB ZB)�B3�B4�B7�B>
B@BF;BKZBL`BL`BS�BX�B_�Bc�Bd�Bf�BjBm"BsGBvZBxfB}�B��B��B��B��B��B��B��B��B��B��B��B��B|�Bv[BrBBn+Bb�BS�BOtBK\B>B/�B#oBB�?B��BӕB�B��B��B��B�/B��BsZBfB_�BX�BIaBDB
�B
�RB
��B
�~B
�B
{�B
oTB
dB
HmB
=+B
0�B
 �B
\B
=B
%B
�B	�PB	� B	�B	��B	�4B	��B	��B	�kB	�_B	�MB	�B	}�B	iBB	W�B	J�B	?IB	7B	5B	+�B	 �B	oB	7B	B��B�pB�WB�EB�3B�9B�@B�9B�!B�B��BϴBǄB�lB�#B��B��B��B��B��B��B��B��B��B�yB�gB�UB�BB�B��B|�Bu�Br�Bo{BlhBhQBc2Bb,B^BV�BT�BT�BY�BW�BW�BU�BS�BR�BQ�BR�BP�BN�BM�BO�BN�BN�BM�BM�BL�BL�BM�BL�BJ�BI�BH�BG�BH�BI�BQ�BM�BM�BI�BF�BD�BCzB>[B:BB71B5$B0B0B2B4B4B4B3B3B3B4B6,B6,B5%B5%B2B0B0B6,B=WBL�BT�BW�BT�BP�BU�BX�B\B\B[B`'Bi]BjcBp�Br�BjdBgRBgRBmvBjdBi^BfKBeEBgRBa.Ba.B]B^B^B^B^B^B`(BgSBp�Bt�Bw�Bw�Bw�Bs�Bs�Br�Br�Bs�Bs�Bs�Bs�Bs�Bt�Bu�By�B}�B�B��B�
B�B�FB�_B�kB�kB�wB��B��B��B��B��B��B��B��B��B�B�B�B�"B�/B�;B�GB�TB�kBƊBʣB͵B��B��B�B�5B�B��B��B	"B	YB	SB	YB	pB	�B	�B	�B	�B	kB	�B	#�B	-�B	1B	2	B	3B	3B	4B	3B	3B	6"B	8.B	::B	?YB	AeB	E}B	H�B	J�B	O�B	Q�B	P�B	N�B	R�B	W�B	X�B	W�B	W�B	U�B	R�B	P�B	O�B	P�B	U�B	V�B	W�B	Y�B	\B	]
B	]
B	]
B	^B	_B	`B	d4B	f@B	gGB	iSB	k_B	mkB	nrB	oxB	p~B	r�B	t�B	v�B	w�B	x�B	z�B	z�B	{�B	}�B	�B	�.B	�"B	�(B	�4B	�_B	�YB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�
B	�B	�B	�#B	�#B	�#B	�#B	�/B	�4B	�AB	�GB	�GB	�GB	�MB	�MB	�_B	�_B	�_B	�lB	�lB	�~B	ǄB	ǄB	ǄB	ȊB	ʗB	ʗB	˝B	˝B	̣B	ͩB	ͩB	ϵB	ϵB	лB	лB	лG�O�B	��B	� B	��B
hB
|B
sB
�B
&B
0�B
6LB
>IB
B-B
HRB
N'B
SB
W�B
\�B
d�B
i0B
nMB
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.14 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144192022020411441920220204114419  AO  ARCAADJP                                                                    20200619170913    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170913  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170913  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114419  IP                  G�O�G�O�G�O�                