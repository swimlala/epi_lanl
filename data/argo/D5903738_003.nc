CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:06:36Z AOML 3.0 creation; 2016-05-31T21:48:57Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230636  20160531144857  5903738 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4053_7107_003                   2C  D   APEX                            5370                            041511                          846 @�1�hL 
1   @�1� ��@8t�j~��c-�-V1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A>ffA^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy� D���D�I�D���D�� D��D�C3D��3D��3D��D�S3D���D�� D�3D�P D�s3D�3D�3D�I�D�3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=p@��
@��
A�A<Q�A\Q�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG�RCI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Dyw�D���D�EqD��qD���D��D�?
D�
D��
D�qD�O
D��qDǻ�D�
D�K�D�o
D�
D�
D�EqD�
D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A��A��!A���A��A��!A��-A��9A��!A��A��-A��!A��A���A���A���A���A���A���A���A��A��A��-A��A�p�A�+A���A��A��HA�ĜA��hA��7A��A�|�A�r�A�bNA�;dA��/A��A�XA�A�A� �A���A���A��wA��A��\A�r�A�n�A�hsA�\)A�=qA�&�A�JA��A��^A���A�v�A�C�A�/A��A���A�t�A�&�A��
A��A���A���A���A��DA�bA�JA�t�A�/A� �A��A��wA�?}A�A�&�A�r�A�  A��A�-A��9A��\A�JA���A�ffA�A���A�\)A���A���A�bA��uA�ffA�S�A�$�A�A�{A���A�=qA�hsA��jA���A���A��\A��9A�A�A��mA�G�A�S�A��A���A�$�A��HA��!A�bNA�5?A�1A���A�p�A�M�A���A�TA{��Au?}Aq��AnA�Ak�wAj-Aep�A`5?A_%A\E�AY?}AX�AW��AW/AT�/AS/AQ�TAP�\AOl�AL��AK�AHȴAF�AE�mAE+AD�AA�FA@�`A@JA=�-A<�uA;�FA9�TA6��A6r�A61A5+A4M�A3�A1oA1\)A1��A1p�A0Q�A/�A.bA-|�A,~�A,�A+��A+?}A*�DA)�
A)�A(�9A((�A'��A&Q�A%C�A$A�A#��A#�A"��A �A��A�A�A��A�9AjA7LA?}AjA?}AƨAC�A-AbA��A�-A�wAO�A��A�DAVA�A�#A
~�A��A�AffA�^A�^A Z@�@�Q�@��@�=q@�r�@�;d@�V@��;@�33@�R@�/@�@��@���@�x�@��@�I�@�S�@���@��@畁@旍@�$�@�u@�$�@���@�V@ܓu@�^5@���@��;@֧�@թ�@�1'@�{@�  @��@��H@��@�x�@���@�l�@Ɨ�@��`@�/@���@�;d@�E�@�?}@���@�  @�C�@�@�E�@���@�1@�C�@��@�ȴ@�^5@���@�O�@�1'@���@���@�E�@�@�@�&�@��@��w@���@�`B@�b@�E�@��h@��@�%@� �@�+@�
=@�@��P@��y@�5?@�hs@�X@�X@��@���@��9@���@��j@��@�Q�@�  @��/@�%@��u@���@��P@�C�@�=q@��y@�E�@�%@�9X@�t�@��\@��`@�%@���@�Z@�o@���@�M�@�?}@�&�@���@���@�j@�9X@�(�@��m@�dZ@�33@���@��\@�v�@�E�@��#@���@�{@��@�J@���@���@�{@�J@�hs@�7L@��@��`@�Q�@��
@�o@��H@��+@�V@��T@��^@��-@��#@��T@�@��@���@�@�{@�V@���@��@�|�@��P@���@���@���@�|�@�dZ@��@��H@��!@�n�@��T@��^@��^@��@�hs@�X@�G�@���@��D@�(�@�1@��m@�ƨ@�;d@�^5@�J@���@�p�@�O�@��@��`@�Ĝ@��`@�%@�V@��@��@���@���@��u@��D@�z�@�Q�@� �@���@���@�l�@�33@�+@�+@�o@��@�o@���@�n�@�v�@�n�@�n�@�v�@��\@�@�o@��@��y@�@��@�
=@�ff@��@��@�?}@�?}@�V@���@��D@�1'@�Q�@��@��@��;@�ƨ@���@��P@��P@�t�@�K�@�S�@��H@�5?@���@��@�O�@��@�z�@�z�@�bN@�Q�@�9X@�1'@��@�1@���@��F@�;d@|�/@uO�@m��@fv�@]��@W�@P��@I��@B��@=�h@7�@0 �@*�@( �@"�!@/@hs@v�@��@l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A��A��!A���A��A��!A��-A��9A��!A��A��-A��!A��A���A���A���A���A���A���A���A��A��A��-A��A�p�A�+A���A��A��HA�ĜA��hA��7A��A�|�A�r�A�bNA�;dA��/A��A�XA�A�A� �A���A���A��wA��A��\A�r�A�n�A�hsA�\)A�=qA�&�A�JA��A��^A���A�v�A�C�A�/A��A���A�t�A�&�A��
A��A���A���A���A��DA�bA�JA�t�A�/A� �A��A��wA�?}A�A�&�A�r�A�  A��A�-A��9A��\A�JA���A�ffA�A���A�\)A���A���A�bA��uA�ffA�S�A�$�A�A�{A���A�=qA�hsA��jA���A���A��\A��9A�A�A��mA�G�A�S�A��A���A�$�A��HA��!A�bNA�5?A�1A���A�p�A�M�A���A�TA{��Au?}Aq��AnA�Ak�wAj-Aep�A`5?A_%A\E�AY?}AX�AW��AW/AT�/AS/AQ�TAP�\AOl�AL��AK�AHȴAF�AE�mAE+AD�AA�FA@�`A@JA=�-A<�uA;�FA9�TA6��A6r�A61A5+A4M�A3�A1oA1\)A1��A1p�A0Q�A/�A.bA-|�A,~�A,�A+��A+?}A*�DA)�
A)�A(�9A((�A'��A&Q�A%C�A$A�A#��A#�A"��A �A��A�A�A��A�9AjA7LA?}AjA?}AƨAC�A-AbA��A�-A�wAO�A��A�DAVA�A�#A
~�A��A�AffA�^A�^A Z@�@�Q�@��@�=q@�r�@�;d@�V@��;@�33@�R@�/@�@��@���@�x�@��@�I�@�S�@���@��@畁@旍@�$�@�u@�$�@���@�V@ܓu@�^5@���@��;@֧�@թ�@�1'@�{@�  @��@��H@��@�x�@���@�l�@Ɨ�@��`@�/@���@�;d@�E�@�?}@���@�  @�C�@�@�E�@���@�1@�C�@��@�ȴ@�^5@���@�O�@�1'@���@���@�E�@�@�@�&�@��@��w@���@�`B@�b@�E�@��h@��@�%@� �@�+@�
=@�@��P@��y@�5?@�hs@�X@�X@��@���@��9@���@��j@��@�Q�@�  @��/@�%@��u@���@��P@�C�@�=q@��y@�E�@�%@�9X@�t�@��\@��`@�%@���@�Z@�o@���@�M�@�?}@�&�@���@���@�j@�9X@�(�@��m@�dZ@�33@���@��\@�v�@�E�@��#@���@�{@��@�J@���@���@�{@�J@�hs@�7L@��@��`@�Q�@��
@�o@��H@��+@�V@��T@��^@��-@��#@��T@�@��@���@�@�{@�V@���@��@�|�@��P@���@���@���@�|�@�dZ@��@��H@��!@�n�@��T@��^@��^@��@�hs@�X@�G�@���@��D@�(�@�1@��m@�ƨ@�;d@�^5@�J@���@�p�@�O�@��@��`@�Ĝ@��`@�%@�V@��@��@���@���@��u@��D@�z�@�Q�@� �@���@���@�l�@�33@�+@�+@�o@��@�o@���@�n�@�v�@�n�@�n�@�v�@��\@�@�o@��@��y@�@��@�
=@�ff@��@��@�?}@�?}@�V@���@��D@�1'@�Q�@��@��@��;@�ƨ@���@��P@��P@�t�@�K�@�S�@��H@�5?@���@��@�O�@��@�z�@�z�@�bN@�Q�@�9X@�1'@��@�1@���@��F@�;d@|�/@uO�@m��@fv�@]��@W�@P��@I��@B��@=�h@7�@0 �@*�@( �@"�!@/@hs@v�@��@l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBB+B#�B&�B)�B,B.B-B-B+B(�B'�B'�B(�B(�B(�B'�B(�B-B1'B2-B33B33B49B5?B5?B6FB6FB6FB6FB5?B49B33B33B6FB6FB6FB8RB7LB49B2-B0!B+B"�B�B�B
=B+BB  B��B�B�/BB�jB�^B�XB�'B��B��B�JBz�B`BBQ�BE�B?}B<jB.B �B�BVB%B��B�)BƨB�LB�B��B��Bz�BM�B8RB1'B"�BbB
��B
�#B
��B
�!B
��B
��B
��B
�PB
�B
w�B
n�B
s�B
q�B
W
B
?}B
F�B
L�B
H�B
B�B
5?B
 �B
%B	�BB	��B	u�B	P�B	<jB	.B	�B	1B	B�B�B��B�B�B�HB�/B��B��B��BǮB�}B�?B�B��B��B��B��B��B��B�hB�oB�bB�PB�B�PB�\B�oB�PB�JB�DB��B��B��B��B��B��B��B��B��B�uB�hB�oB�uB�{B��B��B��B��B�oB��B�{B��B��B�hB�DB|�Bu�B�B�PB��B��B��B�bB�=B�B|�Bv�Bs�Bn�Be`B]/BZBYBYBhsB}�Bz�B[#B@�B:^B8RB=qB9XB8RB8RB;dB<jB<jB=qB=qB?}B@�B@�B@�BA�BC�BD�BE�BF�BF�BF�BF�BF�BE�BE�BH�BS�BP�BL�BJ�BK�BL�BJ�BI�BH�BF�BE�BC�B>wB7LB8RB;dB;dB:^B<jB>wB<jB7LB-B,B-B2-B9XB:^B>wBE�BJ�BL�BO�BR�BS�BT�BT�BT�BZB[#B^5B_;B_;B_;B`BB`BB`BB_;B]/B\)BZBXB\)B]/B`BBcTB`BB^5B_;BcTBiyBiyBhsBjBo�Br�Bv�B{�B�B�1B�7B�bB�bB�hB��B��B��B��B��B��B��B�B�-B�9B�9B�LB�LB�LB�^B�jBBĜBÖBBÖBĜBŢBƨBɺB��B��B��B�B�B�;B�TB�`B�mB�yB�B�B�B��B��B��B��B��B	B	B	B	B	+B	DB	hB	uB	�B	�B	�B	�B	�B	 �B	#�B	&�B	+B	8RB	:^B	:^B	=qB	@�B	G�B	M�B	N�B	O�B	P�B	Q�B	VB	W
B	ZB	]/B	`BB	bNB	gmB	iyB	hsB	iyB	jB	k�B	k�B	m�B	p�B	r�B	s�B	s�B	t�B	u�B	w�B	x�B	y�B	z�B	{�B	{�B	}�B	� B	�B	�+B	�+B	�7B	�JB	�hB	�oB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�FB	�LB	�LB	�XB	�^B	�^B	�^B	�XB	�^B	�jB	�}B	��B	��B	��B	B	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�TB	�B
  B
1B
uB
�B
$�B
,B
49B
;dB
B�B
H�B
M�B
S�B
XB
\)B
`BB
dZB
hsB
l�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B BBB%B+B#�B&�B*	B,B."B-B- B+B)B'�B'�B)B)B)	B'�B)B-B18B2:B3@B3AB4FB5MB5OB6UB6VB6VB6XB5MB4EB3>B3AB6TB6UB6TB8dB7^B4FB2=B0.B+B"�B�B�B
OB8BB B��B�B�<BB�wB�oB�bB�2B��B��B�XBz�B`LBQ�BE�B?�B<vB.#B �B�B`B1B��B�3BƴB�ZB� B�B��Bz�BM�B8`B14B"�BrB
�	B
�1B
��B
�3B
��B
��B
��B
�aB
�"B
w�B
n�B
s�B
q�B
WB
?�B
F�B
L�B
H�B
B�B
5QB
 �B
;B	�VB	��B	u�B	QB	<�B	.1B	�B	QB	=B��B�B��B��B�B�iB�OB�B��B��B��B��B�bB�0B�B��B��B��B��B��B��B��B��B�uB�9B�uB��B��B�uB�pB�iB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�jB}Bu�B�+B�vB��B��B��B��B�`B�/B}Bv�Bs�Bn�Be�B]UBZBBY=BY>Bh�B~B{B[LB@�B:�B8zB=�B9�B8{B8{B;�B<�B<�B=�B=�B?�B@�B@�B@�BA�BC�BD�BE�BF�BF�BF�BF�BF�BE�BE�BH�BT BQBL�BJ�BK�BL�BJ�BI�BH�BF�BE�BC�B>�B7vB8zB;�B;�B:�B<�B>�B<�B7wB-6B,0B-7B2WB9�B:�B>�BE�BJ�BL�BPBSBT BU$BU$BU$BZFB[JB^\B_bB_bB_aB`iB`iB`iB_^B]XB\PBZBBX:B\OB]UB`iBczB`hB^[B_`Bc{Bi�Bi�Bh�Bj�Bo�Br�Bv�B|
B�CB�UB�]B��B��B��B��B��B��B��B��B� B�B�?B�RB�[B�\B�qB�oB�pB��B��B²B��BùB±BøBľB��B��B��B��B��B�B�%B�3B�[B�wB�B�B�B�B�B��B��B��B��B�B�B	&B	+B	,B	9B	IB	cB	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	'B	+ B	8oB	:{B	:{B	=�B	@�B	G�B	M�B	N�B	O�B	QB	R
B	V B	W"B	Z;B	]JB	`^B	bjB	g�B	i�B	h�B	i�B	j�B	k�B	k�B	m�B	p�B	r�B	s�B	s�B	t�B	u�B	w�B	x�B	y�B	z�B	|B	|B	~B	�B	�,B	�GB	�FB	�RB	�gB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�,B	�SB	�^B	�cB	�dB	�pB	�xB	�zB	�yB	�qB	�zB	��B	��B	��B	��B	��B	§B	źB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�*B	�,B	�5B	�5B	�6B	�>B	�lB	��B
 B
IB
�B
�B
$�B
,!B
4OB
;zB
B�B
H�B
M�B
TB
X&B
\@B
`YB
dmB
h�B
l�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.13 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311448572016053114485720160531144857  AO  ARCAADJP                                                                    20140721230636    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230636  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230636  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531144857  IP                  G�O�G�O�G�O�                