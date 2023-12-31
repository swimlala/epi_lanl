CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:03Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170903  20220204114415  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               ,A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ؕN��1   @ؕ��-�@7Z^5?|��c�;dZ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ,A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4fD4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� DlfDl�fDm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�{D� D�Y�D��RD��fD�0RD�P D��3D���D�3D�d)D��3D�ФD�'\D�X DڭD��3D�{D�O
D�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��B�#�B��>B�WB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;�RC=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D�HDw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3�D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO�DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk�Dl~Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt�{Dy�)D��D�UqD��)D��=D�,)D�K�D��
D�θD�
D�` D��
D��{D�#3D�S�Dڨ�D��
D�RD�J�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�VA�JA�JA�VA�VA�JA�bA�oA�{A�{A�oA�{A��A��A��A��A��A��A� �A� �A�"�A�"�A��A��A� �A� �A�"�A�&�A�(�A�&�A��A��A��A���A�ffAžwA��A���A�9XA���A�jA�ƨA���A�K�A�ȴA�M�A�{A���A�ffA�oA���A���A�t�A�hsA�"�A�x�A�;dA�C�A�z�A�O�A�9XA�  A�?}A���A�VA���A�(�A�r�A���A�ZA�A�A��^A��;A���A�$�A���A���A��^A���A�\)A��A�33A�VA�bA��;A���A�M�A�z�A�=qA�-A�;dA���A�`BA��#A�"�A�v�A�\)A�XA�VA��-A�&�A�jA�ĜA���A�A�A�~�A��TA���A�;dA���A�z�A��;A��hA��A��wA�O�A�
=A�1A�A|Az�Awt�Au�Ar�Ao�FAn{Ak�Aj5?Ah��Af�RAct�Ab^5Aa�A`�A_%A]p�A[�hAYhsAW�PAU�#AU;dAR�AP��AO�hAK�;AJ{AGAD��AB^5AA��A@z�A=��A<JA;�A9�A7��A5��A4v�A2�yA0ȴA0ffA/��A/7LA.z�A-+A+�-A*�yA*z�A)�-A);dA(-A&�A%hsA$1'A#XA#/A"��A"1'A!G�A �!A�AS�A7LA�\A�hAVA�A  A��A$�A��A�-A��A��AC�A�\A|�AJA�^A�FA�-A\)A9XA��AS�A�A
��A	�PAbA/A�A^5A-A��A�wA�A�At�A7LA ��@�S�@�J@�I�@�^5@��u@�Q�@��m@�+@���@�@�o@�ff@�$�@�@��/@�bN@�S�@�!@�h@�j@��@�\@���@�Q�@畁@�n�@噚@�@�l�@���@߶F@�+@�-@�j@�@��@�ƨ@�V@���@��/@�z�@�A�@�  @҇+@�Ĝ@���@�C�@�o@Χ�@���@�x�@���@���@� �@��y@�v�@��@ɩ�@���@ȃ@�K�@��@ũ�@őh@�hs@�Ĝ@�Q�@���@�@�~�@��@��@���@��`@�1'@���@���@�@��@���@���@���@�9X@�C�@���@�E�@���@���@�j@��;@�33@���@�=q@�/@���@�ȴ@�J@���@��^@�X@�bN@���@�33@���@��^@��@��@�Z@�t�@��+@��@��@��/@���@��@�1'@��
@��@�K�@���@��R@�M�@�{@�$�@�5?@�-@�E�@�$�@���@���@�p�@�`B@�7L@���@��u@��m@�t�@�~�@�-@��-@��@���@��`@� �@�\)@�
=@�ff@�J@��T@���@�@�x�@�?}@��j@���@�x�@�x�@�X@���@�E�@�$�@��T@���@��^@�x�@��j@�9X@�M�@���@�t�@�33@��y@��@�^5@��^@��T@�{@��@���@�x�@��@��`@��j@�I�@���@�ƨ@�|�@�o@��R@�~�@�=q@�$�@�J@��T@���@�X@���@�A�@�b@��
@��F@���@�t�@�S�@�+@���@�M�@�@��@�9X@���@�K�@�ȴ@�-@�=q@�t�@�S�@��!@��\@��+@��+@�~�@�ff@�V@�E�@�5?@�-@�-@�-@�$�@�{@���@���@���@�hs@�O�@�7L@�7L@�/@��@���@���@�Z@�Q�@�A�@�b@���@��P@�l�@�33@���@�~�@���@��9@��j@��9@��D@��D@��`@��@���@���@��@�bN@�bN@�bN@�b@��@�t�@��N@vJ�@m�@f^5@`bN@U��@N͟@K)_@DK^@=e,@7\)@0��@,-�@%j@�:@��@�@C@R�@
C�@�g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�VA�JA�JA�VA�VA�JA�bA�oA�{A�{A�oA�{A��A��A��A��A��A��A� �A� �A�"�A�"�A��A��A� �A� �A�"�A�&�A�(�A�&�A��A��A��A���A�ffAžwA��A���A�9XA���A�jA�ƨA���A�K�A�ȴA�M�A�{A���A�ffA�oA���A���A�t�A�hsA�"�A�x�A�;dA�C�A�z�A�O�A�9XA�  A�?}A���A�VA���A�(�A�r�A���A�ZA�A�A��^A��;A���A�$�A���A���A��^A���A�\)A��A�33A�VA�bA��;A���A�M�A�z�A�=qA�-A�;dA���A�`BA��#A�"�A�v�A�\)A�XA�VA��-A�&�A�jA�ĜA���A�A�A�~�A��TA���A�;dA���A�z�A��;A��hA��A��wA�O�A�
=A�1A�A|Az�Awt�Au�Ar�Ao�FAn{Ak�Aj5?Ah��Af�RAct�Ab^5Aa�A`�A_%A]p�A[�hAYhsAW�PAU�#AU;dAR�AP��AO�hAK�;AJ{AGAD��AB^5AA��A@z�A=��A<JA;�A9�A7��A5��A4v�A2�yA0ȴA0ffA/��A/7LA.z�A-+A+�-A*�yA*z�A)�-A);dA(-A&�A%hsA$1'A#XA#/A"��A"1'A!G�A �!A�AS�A7LA�\A�hAVA�A  A��A$�A��A�-A��A��AC�A�\A|�AJA�^A�FA�-A\)A9XA��AS�A�A
��A	�PAbA/A�A^5A-A��A�wA�A�At�A7LA ��@�S�@�J@�I�@�^5@��u@�Q�@��m@�+@���@�@�o@�ff@�$�@�@��/@�bN@�S�@�!@�h@�j@��@�\@���@�Q�@畁@�n�@噚@�@�l�@���@߶F@�+@�-@�j@�@��@�ƨ@�V@���@��/@�z�@�A�@�  @҇+@�Ĝ@���@�C�@�o@Χ�@���@�x�@���@���@� �@��y@�v�@��@ɩ�@���@ȃ@�K�@��@ũ�@őh@�hs@�Ĝ@�Q�@���@�@�~�@��@��@���@��`@�1'@���@���@�@��@���@���@���@�9X@�C�@���@�E�@���@���@�j@��;@�33@���@�=q@�/@���@�ȴ@�J@���@��^@�X@�bN@���@�33@���@��^@��@��@�Z@�t�@��+@��@��@��/@���@��@�1'@��
@��@�K�@���@��R@�M�@�{@�$�@�5?@�-@�E�@�$�@���@���@�p�@�`B@�7L@���@��u@��m@�t�@�~�@�-@��-@��@���@��`@� �@�\)@�
=@�ff@�J@��T@���@�@�x�@�?}@��j@���@�x�@�x�@�X@���@�E�@�$�@��T@���@��^@�x�@��j@�9X@�M�@���@�t�@�33@��y@��@�^5@��^@��T@�{@��@���@�x�@��@��`@��j@�I�@���@�ƨ@�|�@�o@��R@�~�@�=q@�$�@�J@��T@���@�X@���@�A�@�b@��
@��F@���@�t�@�S�@�+@���@�M�@�@��@�9X@���@�K�@�ȴ@�-@�=q@�t�@�S�@��!@��\@��+@��+@�~�@�ff@�V@�E�@�5?@�-@�-@�-@�$�@�{@���@���@���@�hs@�O�@�7L@�7L@�/@��@���@���@�Z@�Q�@�A�@�b@���@��P@�l�@�33@���@�~�@���@��9@��j@��9@��D@��D@��`@��@���@���@��@�bN@�bN@�bN@�b@��G�O�@��N@vJ�@m�@f^5@`bN@U��@N͟@K)_@DK^@=e,@7\)@0��@,-�@%j@�:@��@�@C@R�@
C�@�g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B� B~�B~�B~�B~�B~�B~�B� B� B� B� B� B�B{�Bv�Bp�Bq�Bu�Bv�B~�B� B� B� B~�B}�B�B�B�B�B�B�B�B�B�B�%B�+B�1B�%B�%B�%B�B�B�B�B�B�B}�Bz�Br�Bm�BdZB`BB\)BYBQ�BG�BF�BA�B:^B/B)�B�B�B�BuB+B��B�yB�#B��B��BĜB�B�uB�Bt�Bo�BjBbNBVBC�B5?B"�B�B\BB
��B
�B
�B
�RB
��B
�VB
}�B
�DB
�B
u�B
^5B
C�B
33B
�B
\B	�B	�#B	�TB	��B	B	�-B	��B	�+B	�VB	��B	�\B	�+B	w�B	iyB	XB	L�B	@�B	;dB	33B	&�B	$�B	B�B�/B��B�wB�LB�FB�'B��B��B�VB�\B�7B{�Bu�Bu�Bt�Bt�Bs�Bq�Bp�Bm�Bm�Bm�Bn�Bl�Bl�Bm�Bp�Bl�Bk�BiyBk�BjBk�BhsBffBcTBbNBaHB_;B^5BZBYBVBT�BT�BQ�BQ�BO�BN�BL�BJ�BJ�BH�BH�BG�BG�BG�BE�BD�BC�BB�BB�B@�B@�B?}B=qB=qB<jB<jB9XB;dB8RB8RB6FB9XB7LB7LB:^B8RB8RB7LB6FB7LB6FB6FB7LB7LB7LB7LB7LB8RB8RB8RB9XB9XB9XB9XB9XB9XB:^B9XB8RB7LB:^B9XB9XB9XB;dB<jB;dB@�B@�B@�BA�BA�BA�BA�BD�BF�BH�BH�BH�BI�BJ�BK�BK�BK�BM�BP�BP�BQ�BR�BT�BT�BYB\)B]/B]/B]/B_;B`BBbNBdZBe`BffBffBgmBiyBk�Bk�Bm�Bp�Br�Bs�Br�Br�Bs�Bu�Bv�Bw�Bx�Bz�Bz�B|�B� B�B�B�DB�PB�hB�hB�oB�uB��B��B��B��B��B��B�B�B�B�B�!B�9B�^B�}B��BÖBǮBɺB��B��B��B�B�;B�TB�mB�B�B	B	
=B	JB	VB	bB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	)�B	/B	1'B	2-B	49B	5?B	6FB	7LB	;dB	?}B	C�B	L�B	Q�B	R�B	XB	\)B	`BB	cTB	e`B	ffB	gmB	hsB	gmB	hsB	e`B	cTB	cTB	cTB	cTB	cTB	cTB	iyB	n�B	p�B	q�B	s�B	v�B	w�B	y�B	}�B	~�B	� B	�B	�B	�B	�B	�%B	�+B	�+B	�1B	�7B	�=B	�DB	�VB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�9B	�?B	�FB	�XB	�dB	�}B	��B	ÖB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�
B	�B	�B	�)B	�/B	�;B	�BB	�BB	�HB	�NB	�TB	�NB	�NB	�HB	�B	��B
�B
�B
�B
#�B
,�B
0�B
5�B
@�B
GzB
MB
Q�B
W�B
^�B
a|B
f�B
k�B
o�B
s�B
ut111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bw�Bv�Bv�Bv�Bv�Bv�Bv�Bw�Bw�Bw�Bw�Bw�Bx�Bs~BnaBh=BiCBm\BnbBv�Bw�Bw�Bw�Bv�Bu�Bx�Bx�By�By�Bz�Bz�Bz�Bz�B{�B}�B~�B�B}�B}�B}�B|�B{�By�B|�Bx�By�Bu�Br}BjLBe.B[�BW�BS�BP�BI�B?NB>HB9)B1�B&�B!�BVB>B+BB��B�oB�!B��BƂB�kB�FB��B�#B{�BllBgOBb0BZ BM�B;JB,�B�B?BB
��B
�B
�@B
��B
�B
�rB
�B
u�B
�B
z�B
m�B
U�B
;]B
*�B
�B
'B	�rB	��B	�#B	��B	�`B	� B	��B	B	�+B	�\B	�2B	B	o�B	aQB	O�B	D�B	8_B	3@B	+B	�B	�B��B�B�B��B�]B�2B�-B�B��B�|B�@B�FB�"Bs�Bm�Bm�Bl�Bl�Bk�Bi�Bh�BeBeBeBf�BdyBdyBeBh�BdzBctBahBctBbnBctB`bB^VB[DBZ>BY8BW+BV%BRBQBM�BL�BL�BI�BI�BG�BF�BD�BB�BB�B@�B@�B?�B?�B?�B=�B<�B;�B:�B:�B8xB8xB7rB5fB5fB4_B4`B1NB3ZB0HB0HB.<B1NB/CB/CB2UB0IB0IB/CB.=B/CB.=B.=B/CB/CB/CB/CB/DB0JB0JB0JB1PB1PB1PB1PB1PB1PB2VB1PB0JB/DB2VB1QB1QB1QB3]B4cB3]B8|B8|B8|B9�B9�B9�B9�B<�B>�B@�B@�B@�BA�BB�BC�BC�BC�BE�BH�BH�BI�BJ�BL�BL�BQBT!BU'BU'BU'BW3BX:BZFB\RB]XB^^B^^B_eBaqBc}Bc}Be�Bh�Bj�Bk�Bj�Bj�Bk�Bm�Bn�Bo�Bp�Br�Br�Bt�Bw�BzB}B�:B�FB�^B�^B�eB�kB�wB��B��B��B��B��B��B��B�B�	B�B�.B�RB�qB�wB��B��B��BúB��B��B�
B�-B�FB�_B�vB�B�B	-B	9B	EB	QB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	'	B	)B	*B	,'B	--B	.3B	/9B	3QB	7jB	;�B	D�B	I�B	J�B	O�B	TB	X-B	[?B	]KB	^PB	_WB	`]B	_XB	`^B	]KB	[?B	[?B	[?B	[?B	[?B	[?B	adB	f�B	h�B	i�B	k�B	n�B	o�B	q�B	u�B	v�B	w�B	x�B	z�B	|B	|B	~B	B	B	�B	� B	�&B	�-B	�?B	�XB	�dB	�jB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�&B	�-B	�?B	�KB	�cB	�iB	�|B	��B	��B	§B	§B	ŹB	ƾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�'B	�'B	�-B	�3B	�9B	�3B	�3G�O�B	��B	�B	��B

�B
�B
�B
$mB
(�B
-�B
8�B
?[B
D�B
I�B
O�B
V~B
Y\B
^�B
c�B
g�B
k�B
mT111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.13 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144152022020411441520220204114415  AO  ARCAADJP                                                                    20200619170903    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170903  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170903  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114415  IP                  G�O�G�O�G�O�                