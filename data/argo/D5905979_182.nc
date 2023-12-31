CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-10-28T01:02:07Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20201028010207  20220204114429  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�A�����1   @�A�/hZ2@5��$�/�b�5?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�33B���B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4�fD5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� DdfDd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDt� Dt��Dy� D�D�iHD��D��qD�"=D�c�D��qD��)D��D�X�D��)D��3D�RD�^Dڑ�D��)D�\D�O�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@��@��AA=A]A}A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�B�
Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bw�
B�
B��B��RB��RB��RB��RB��RB�Q�B�Q�B��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK��CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)CeCg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
D�pDw
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
D4}pD4�
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
DS�pDTw
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
Dc�pDdw
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
Ds�pDtw
Dt��Dy�
D�	�D�d�D���D���D��D�_
D���D�׮D�3D�T)D���D�޸D��D�Y�DڍD��D��D�K3D�\D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aӟ�Aӡ�Aӡ�AӬAӬAӬAө�Aө�Aӣ�Aӧ�Aӣ�AӅA�|�A�t�A�t�A�r�A�r�A�p�A�t�A�t�A�v�A�v�A�x�AӁA���A��A�A�A�ffA��A���A���A�ȴA��A��A�$�A��AĶFA�$�A�ƨA��A��yA���A���A��+A�9XA��A��hA��yA�C�A�ĜA�\)A�JA��A�;dA��
A�VA�~�A��uA��hA��!A�?}A��A�I�A�bA��uA�
=A���A�(�A���A�p�A�A��/A� �A���A�dZA���A�`BA�ĜA�A�bNA��#A�E�A���A��^A���A�A�A�"�A���A�(�A���A� �A���A���A���A���A���A�1A���A��A��hA�n�A�7LA���A���A���A�E�A�bA�+A�9XA��HA�z�A�`BA���A���A��^A���A��RA��^A+A}�Az�9Ax�yAvA�At�/Aq�7An5?AlJAiC�Af$�Aa�mA_�#A[dZAXjAW&�AS�;AP�/AO�AM�7AK�AJ5?AH^5AF�yAEXAD��AC�ABVA@��A@ZA>�A<��A:ȴA8ffA6�A5��A4�A3S�A2ZA1��A1��A0��A0�A.��A.Q�A-�PA,�`A+�7A)��A)K�A(��A'�A&-A%/A$v�A#C�A"bA bNA�A�+A�PA��A�DA�-A�A�A�A�hA?}AbNA"�A��A��A=qA�;AhsA�`AffA?}A�^A?}AĜAƨAr�A;dA	|�A��A�TAt�A~�A7LA1'AĜA-A�A ��A�@��m@�n�@���@��`@�ƨ@�|�@�X@��w@�v�@��@�G�@�z�@���@�@�X@�Q�@�ȴ@�(�@��;@�o@��@�@�1'@��@��@�Q�@�ƨ@��@�9X@�
=@�9X@�\)@�"�@ڟ�@ف@��
@���@�ƨ@���@�V@� �@�dZ@��H@�X@�ƨ@�t�@�@�^5@ɩ�@���@�z�@Ų-@�z�@�b@�+@�V@�bN@��@��@�ȴ@���@�V@��@�O�@�(�@�
=@�O�@���@���@���@���@�&�@�1@���@�+@�ȴ@�M�@�@�x�@�&�@��/@���@��@���@���@�J@�X@��D@�1@�|�@���@�^5@���@���@���@��@�33@��@���@�$�@�@���@��@�j@���@��@�S�@��y@�v�@�5?@��@��h@�X@�/@��@�9X@��P@�|�@�t�@�S�@��@��!@�=q@��@��@��@���@�@��^@���@��@��@�Ĝ@��`@�%@���@�r�@�(�@�1@��@��
@���@�|�@�K�@���@�ff@�@��@��@���@���@�hs@�%@���@��9@���@�j@�9X@� �@�  @��m@��F@�"�@�o@�"�@��H@��R@�=q@�@��T@���@�@�@���@��@�-@�ff@��T@��^@��-@�n�@��\@��R@���@��R@�
=@�C�@�ȴ@�V@�-@���@�`B@�G�@�%@�j@� �@��@�1'@�I�@��P@���@��@�&�@���@�V@��`@���@��@�r�@��^@�5?@�hs@���@�7L@��P@��;@���@�%@��`@�j@��@�Ĝ@��@�1@�ƨ@��F@��m@��;@��P@�C�@�~�@�`B@�G�@��-@��T@���@�{@��@�J@���@���@�p�@�V@���@�V@��@���@�z�@�r�@�Q�@�  @��F@��P@�"�@��+@�n�@��h@�/@���@��@���@���@��@��@�  @�@�ȴ@��\@�~�@�^5@�J@��@���@��h@�_p@}B�@r͟@j��@_�+@[�@R^5@J�,@A�Z@=�@6ff@.�y@)[W@%�z@#��@A�@��@��@��@�@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aӟ�Aӡ�Aӡ�AӬAӬAӬAө�Aө�Aӣ�Aӧ�Aӣ�AӅA�|�A�t�A�t�A�r�A�r�A�p�A�t�A�t�A�v�A�v�A�x�AӁA���A��A�A�A�ffA��A���A���A�ȴA��A��A�$�A��AĶFA�$�A�ƨA��A��yA���A���A��+A�9XA��A��hA��yA�C�A�ĜA�\)A�JA��A�;dA��
A�VA�~�A��uA��hA��!A�?}A��A�I�A�bA��uA�
=A���A�(�A���A�p�A�A��/A� �A���A�dZA���A�`BA�ĜA�A�bNA��#A�E�A���A��^A���A�A�A�"�A���A�(�A���A� �A���A���A���A���A���A�1A���A��A��hA�n�A�7LA���A���A���A�E�A�bA�+A�9XA��HA�z�A�`BA���A���A��^A���A��RA��^A+A}�Az�9Ax�yAvA�At�/Aq�7An5?AlJAiC�Af$�Aa�mA_�#A[dZAXjAW&�AS�;AP�/AO�AM�7AK�AJ5?AH^5AF�yAEXAD��AC�ABVA@��A@ZA>�A<��A:ȴA8ffA6�A5��A4�A3S�A2ZA1��A1��A0��A0�A.��A.Q�A-�PA,�`A+�7A)��A)K�A(��A'�A&-A%/A$v�A#C�A"bA bNA�A�+A�PA��A�DA�-A�A�A�A�hA?}AbNA"�A��A��A=qA�;AhsA�`AffA?}A�^A?}AĜAƨAr�A;dA	|�A��A�TAt�A~�A7LA1'AĜA-A�A ��A�@��m@�n�@���@��`@�ƨ@�|�@�X@��w@�v�@��@�G�@�z�@���@�@�X@�Q�@�ȴ@�(�@��;@�o@��@�@�1'@��@��@�Q�@�ƨ@��@�9X@�
=@�9X@�\)@�"�@ڟ�@ف@��
@���@�ƨ@���@�V@� �@�dZ@��H@�X@�ƨ@�t�@�@�^5@ɩ�@���@�z�@Ų-@�z�@�b@�+@�V@�bN@��@��@�ȴ@���@�V@��@�O�@�(�@�
=@�O�@���@���@���@���@�&�@�1@���@�+@�ȴ@�M�@�@�x�@�&�@��/@���@��@���@���@�J@�X@��D@�1@�|�@���@�^5@���@���@���@��@�33@��@���@�$�@�@���@��@�j@���@��@�S�@��y@�v�@�5?@��@��h@�X@�/@��@�9X@��P@�|�@�t�@�S�@��@��!@�=q@��@��@��@���@�@��^@���@��@��@�Ĝ@��`@�%@���@�r�@�(�@�1@��@��
@���@�|�@�K�@���@�ff@�@��@��@���@���@�hs@�%@���@��9@���@�j@�9X@� �@�  @��m@��F@�"�@�o@�"�@��H@��R@�=q@�@��T@���@�@�@���@��@�-@�ff@��T@��^@��-@�n�@��\@��R@���@��R@�
=@�C�@�ȴ@�V@�-@���@�`B@�G�@�%@�j@� �@��@�1'@�I�@��P@���@��@�&�@���@�V@��`@���@��@�r�@��^@�5?@�hs@���@�7L@��P@��;@���@�%@��`@�j@��@�Ĝ@��@�1@�ƨ@��F@��m@��;@��P@�C�@�~�@�`B@�G�@��-@��T@���@�{@��@�J@���@���@�p�@�V@���@�V@��@���@�z�@�r�@�Q�@�  @��F@��P@�"�@��+@�n�@��h@�/@���@��@���@���@��@��@�  @�@�ȴ@��\@�~�@�^5@�J@��@���G�O�@�_p@}B�@r͟@j��@_�+@[�@R^5@J�,@A�Z@=�@6ff@.�y@)[W@%�z@#��@A�@��@��@��@�@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBD�BD�BC�BC�BC�BC�BC�BC�BB�BB�BC�BB�BA�BA�BA�BB�BB�BB�BC�BC�BD�BE�BF�BJ�Bl�B�uB��BBɺB�BB%B�B��B�B�RB�BL�B5?BM�BT�BgmBr�B{�B�B�+B�PB��B��B�BB�#B�`B�sB�B��BVBoB#�B8RB<jB;dBA�BF�BG�BH�BH�BF�BK�BM�BN�BO�BVBW
B[#BZBT�BS�BR�BN�BH�BC�B9XB+B�B�B
=B��B�B��B�B�HB��BÖB�FB��B��B�oB�7Bs�B]/BJ�B49B�BoB1B
��B
�fB
�B
��B
ǮB
�RB
��B
�+B
{�B
gmB
]/B
K�B
<jB
)�B
�B
	7B	��B	�B	�B	�XB	��B	�uB	� B	bNB	N�B	7LB	!�B	�B	1B��B�B�ZB�#B��BȴB��B�XB�?B�'B�B��B��B��B�hB�DB�B|�Bw�Bv�Bt�Bq�Bo�Bo�Bm�Bk�BjBiyBiyBl�Bl�BiyBhsBgmBbNB]/BYBVBXBR�BQ�BK�BI�BI�BC�BE�BA�BA�BG�BG�BJ�BR�BZBS�BVB_;B^5B[#B^5BhsBhsBffBgmBn�Bm�Bk�BgmBe`B_;B^5B[#BYBW
BVB\)BVBYBZB`BBhsBiyBjBv�Bu�Bs�B{�B~�B� B�B�B�B�B�B�B�B�B�JB�PB�\B�hB�oB�hB�bB�VB�JB�JB�=B�1B�%B�%B�%B�%B�7B�\B�VB�JB�=B�B}�B|�B~�B� B� B~�B}�B�B�1B�DB�PB�VB�VB�oB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�'B�!B�9B�3B�9B�RB�XB�jB�wB��BĜBŢBȴBɺB��B��B��B��B��B��B��B�
B�#B�5B�NB�NB�ZB�fB�mB�B�B�B�B��B��B��B��B��B	B	B	1B	PB	\B	�B	�B	 �B	#�B	#�B	#�B	$�B	'�B	(�B	,B	.B	/B	1'B	49B	7LB	:^B	<jB	<jB	>wB	@�B	E�B	J�B	K�B	L�B	N�B	O�B	O�B	P�B	Q�B	S�B	W
B	]/B	aHB	bNB	dZB	ffB	iyB	iyB	l�B	p�B	p�B	q�B	q�B	s�B	v�B	w�B	x�B	{�B	� B	� B	�B	�B	�1B	�=B	�JB	�JB	�VB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�LB	�^B	�jB	�dB	�jB	�jB	�jB	�jB	�dB	�dB	�wB	�wB	�}B	��B	�}B	�dB	�^B	�FB	�FB	�^B	�qB	�jB	�jB	�wB	ǮB	��B	��B	��B	��B	ƨB	ȴB	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�#B	�/B	�)B	�#B	�#B	�B	�#B	�5B	�BB	�HB	�HB	�HB	�NB	�ZB	�`B	�`B	�`B	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
mB
�B
 \B
*B
2B
:�B
A�B
I�B
M�B
UgB
\B
`BB
dZB
gmB
kB
p�B
tB
x�B
|�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B4SB4SB3MB3MB3MB3MB3MB3MB2FB2FB3MB2FB1AB1AB1AB2GB2GB2GB3MB3MB4SB5YB6_B:wB\=B�"B��B�7B�aB�@B��B��B��B��B�TB��Bq�B<�B$�B=�BD�BW(BbiBk�Bp�Bv�B}B�=B�zB��B�@B��B�B� B�JB�zB��BB~B'�B,B+B1-B6KB7RB8XB8XB6LB;kB=vB>|B?�BE�BF�BJ�BI�BD�BC�BB�B>~B8ZB3=B) B�BjB	GB��B�uB�cB�B�iB��B��B�PB�B��B�`B�0Bx�Bc{BL�B:�B$B�BBB
�B
�B
�@B
��B
��B
��B
�2B
��B
wB
k�B
WYB
MB
;�B
,\B
�B
�B	�0B	�B	�}B	�B	�]B	��B	��B	pB	R`B	>�B	'eB	�B	
�B�QB��BܯB�B�JB�B��B��B��B�lB�UB�<B�B��B��B��B{xBsHBm%BhBgBd�Ba�B_�B_�B]�B[�BZ�BY�BY�B\�B\�BY�BX�BW�BR�BMmBIVBFCBHOBC2BB-B<	B9�B9�B3�B5�B1�B1�B7�B7�B;BC4BJ_BD;BFGBO}BNwBKeBNwBX�BX�BV�BW�B^�B]�B[�BW�BU�BOBNyBKhBI\BGPBFJBLoBFKBI^BJdBP�BX�BY�BZ�BgBfBc�Bl*Bo=BpCBqIBqIBqJBrPBrPBqJBqJBsVB|�B}�B�B��B��B��B��B~�B|�B|�Bz�BxvBvjBvkBvkBvkBy}B�B~�B|�Bz�BrSBn<Bm6BoBBpHBpHBoCBn=BsZBxyB{�B}�B~�B~�B��B��B��B��B��B��B�B�B�B�B�B�B�#B�6B�<B�HB�ZB�fB�lB�mB�gB�~B�yB�B��B��B��B��B��B��B��B��B��B�
B�B�"B�"B�(B�(B�5B�LB�eB�wBҏBҏBԛB֧B׮B��B��B��B��B��B�B�B� B�3B�JB�WB�oB��B��B	�B		�B	 B	B	B	B	B	*B	0B	BB	NB	TB	!`B	$rB	'�B	*�B	,�B	,�B	.�B	0�B	5�B	:�B	;�B	=B	?B	@B	@B	AB	B"B	D-B	G?B	McB	Q|B	R�B	T�B	V�B	Y�B	Y�B	\�B	`�B	`�B	a�B	a�B	c�B	f�B	h B	iB	lB	p1B	p1B	q6B	uOB	xaB	zlB	|yB	|yB	~�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�3B	�?B	�LB	�RB	�]B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�qB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�%B	�+B	�1B	�1B	�1B	�7B	�JB	�UB	�OB	�JB	�JB	�DB	�JB	�[B	�hB	�nB	�nB	�nB	�tB	ԀB	ՆB	ՆB	ՆB	ՆB	֌B	ٞB	ڤB	۪B	۪B	ܰB	ݶB	޽B	޽B	��B	��B	��B	��B	޽B	޽B	޽B	޽B	޽B	޽B	��B	޽B	۫B	۫B	ܱB	ܱB	ܱB	ݷB	޽B	޽G�O�B	��B	��B
	B
~B
�B
"3B
*�B
1�B
9�B
=�B
E�B
L-B
P`B
TxB
W�B
[B
`�B
d<B
h�B
l�B
q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.14 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.016(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144292022020411442920220204114429  AO  ARCAADJP                                                                    20201028010207    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20201028010207    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20201028010207  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20201028010207  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114429  IP                  G�O�G�O�G�O�                