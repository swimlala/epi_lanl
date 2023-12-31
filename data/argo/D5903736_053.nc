CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:34Z AOML 3.0 creation; 2016-05-31T19:14:33Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230534  20160531121433  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               5A   AO  4051_7090_053                   2C  D   APEX                            5368                            041511                          846 @ֹ>@1   @ֹ?�@4�;dZ��e^5?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    5A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BY��B^��Bg��Bp  Bx  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dys3D���D�I�D��3D�ٚD�fD�S3D���D��3D�3D�33D�s3D��fD��D�@ DڦfD๚D��fD�)�D�i�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @w�@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BY{B^G�Bg{Boz�Bwz�Bz�B��qB��B��B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB��B��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG�DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Dyj�D��qD�EqD��
D��qD�=D�O
D���D��
D��
D�/
D�o
D��=D��D�;�Dڢ=D�qD��=D�%qD�eqD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A޸RA�jA��A�1A��A��TA��
A���A�ĜAݾwAݾwAݶFAݰ!AݮAݥ�Aݡ�Aݟ�Aݛ�A�z�A�^5A�G�A�+A���A��A�
=AՋDA�1A�oA�|�A��`A�  A��FA��wA�bNA���A��A��A�v�A��wA��DA�`BA���A�M�A��A��wA���A�~�A��A���A�r�A�I�A� �A���A���A���A�x�A��PA� �A���A���A���A�`BA�&�A���A��;A��;A���A��9A�  A�x�A�+A���A��mA�bNA��HA�`BA�`BA�1A�(�A��#A�S�A���A�"�A���A�hsA��9A�+A���A�jA��PA�K�A���A��#A��!A���A��A�VA�$�A��A���A|�Ax��At��As?}AqG�Ao/An$�Am�^Am��Am�7Amt�Am+Al �Aj�AbA�A`VA_t�A^I�A\jAZ��AY��AXE�AU\)AP�`APM�AO��AO��AOO�AN��ANM�AM�PAM\)AL�uAI�wAHr�AG;dAEhsAC��AB�AB�\AA;dA>n�A9`BA5�
A5��A5�PA5dZA5\)A5C�A4�A4��A4��A4��A4�jA4=qA1�mA0�!A/�7A/7LA/"�A/%A/%A.�A.9XA,��A*�jA(ĜA((�A'C�A%��A#��A"5?A!�FA �\A�9A�A/A7LAbNA��A�RA  A�AZA;dA�A��A
=A9XA+A�
A
��A	K�A~�A�A?}A�9AK�A��A�RAt�A�TA z�@�K�@�G�@���@���@��@���@��
@��@���@���@�j@� �@�t�@��@��;@���@��@�33@�ff@���@���@�7L@�1'@�33@�v�@�7L@��@�5?@��m@�@��u@�9X@�  @ߍP@���@ۮ@�ff@���@�(�@�K�@���@���@�?}@���@Ԭ@� �@ӝ�@ҧ�@�J@�p�@�r�@��m@�o@͑h@�j@�ƨ@�"�@���@��`@ǅ@�ȴ@�X@�|�@�+@�1'@���@���@��@�K�@�+@�o@���@���@��H@��@���@�ff@��@���@�%@��
@�33@���@�V@�5?@��@��T@���@��^@�?}@��`@�Z@��w@�"�@��y@�n�@�5?@��^@�p�@�V@��@�(�@��w@��@��y@��7@���@���@��9@��@�I�@� �@�|�@�@�v�@���@��h@�p�@��9@��@�"�@��H@�ff@�x�@��j@��D@��@�\)@�K�@��@���@�{@�&�@� �@���@�t�@�dZ@�l�@�t�@��@�l�@��@�E�@��#@��^@�O�@�&�@��@�9X@��;@���@��w@���@�|�@�K�@�
=@��R@���@���@���@��!@���@��+@�n�@�E�@�5?@�-@�$�@�$�@�$�@��@�{@�@���@��#@��-@�`B@��@���@���@�j@�Q�@�A�@� �@�  @��@��@��@��R@��\@�n�@�5?@��@�@��^@���@��7@��@�O�@�%@���@��j@��u@��D@��@�r�@�Z@�1'@�  @��m@��
@�|�@�t�@�t�@�dZ@�C�@�"�@�ȴ@�^5@��T@��7@�`B@�?}@��/@�z�@�b@�ƨ@�dZ@�33@���@���@���@�~�@�v�@�M�@�@���@��@�X@�7L@�&�@��@�Ĝ@�Q�@�1'@�(�@� �@���@�l�@�\)@�33@���@�=q@�5?@�J@���@�O�@�/@��@��`@���@�Ĝ@�Ĝ@���@��@���@�A�@��w@�;d@��!@��+@�v�@�ff@�V@�@�@��h@��h@���@�hs@�`B@��@��`@��9@�z�@�S�@|��@o�@fȴ@_|�@X��@S33@L��@E?}@=�@7�P@1�@';d@!��@9X@\)@"�@l�@ƨ@
M�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A޸RA�jA��A�1A��A��TA��
A���A�ĜAݾwAݾwAݶFAݰ!AݮAݥ�Aݡ�Aݟ�Aݛ�A�z�A�^5A�G�A�+A���A��A�
=AՋDA�1A�oA�|�A��`A�  A��FA��wA�bNA���A��A��A�v�A��wA��DA�`BA���A�M�A��A��wA���A�~�A��A���A�r�A�I�A� �A���A���A���A�x�A��PA� �A���A���A���A�`BA�&�A���A��;A��;A���A��9A�  A�x�A�+A���A��mA�bNA��HA�`BA�`BA�1A�(�A��#A�S�A���A�"�A���A�hsA��9A�+A���A�jA��PA�K�A���A��#A��!A���A��A�VA�$�A��A���A|�Ax��At��As?}AqG�Ao/An$�Am�^Am��Am�7Amt�Am+Al �Aj�AbA�A`VA_t�A^I�A\jAZ��AY��AXE�AU\)AP�`APM�AO��AO��AOO�AN��ANM�AM�PAM\)AL�uAI�wAHr�AG;dAEhsAC��AB�AB�\AA;dA>n�A9`BA5�
A5��A5�PA5dZA5\)A5C�A4�A4��A4��A4��A4�jA4=qA1�mA0�!A/�7A/7LA/"�A/%A/%A.�A.9XA,��A*�jA(ĜA((�A'C�A%��A#��A"5?A!�FA �\A�9A�A/A7LAbNA��A�RA  A�AZA;dA�A��A
=A9XA+A�
A
��A	K�A~�A�A?}A�9AK�A��A�RAt�A�TA z�@�K�@�G�@���@���@��@���@��
@��@���@���@�j@� �@�t�@��@��;@���@��@�33@�ff@���@���@�7L@�1'@�33@�v�@�7L@��@�5?@��m@�@��u@�9X@�  @ߍP@���@ۮ@�ff@���@�(�@�K�@���@���@�?}@���@Ԭ@� �@ӝ�@ҧ�@�J@�p�@�r�@��m@�o@͑h@�j@�ƨ@�"�@���@��`@ǅ@�ȴ@�X@�|�@�+@�1'@���@���@��@�K�@�+@�o@���@���@��H@��@���@�ff@��@���@�%@��
@�33@���@�V@�5?@��@��T@���@��^@�?}@��`@�Z@��w@�"�@��y@�n�@�5?@��^@�p�@�V@��@�(�@��w@��@��y@��7@���@���@��9@��@�I�@� �@�|�@�@�v�@���@��h@�p�@��9@��@�"�@��H@�ff@�x�@��j@��D@��@�\)@�K�@��@���@�{@�&�@� �@���@�t�@�dZ@�l�@�t�@��@�l�@��@�E�@��#@��^@�O�@�&�@��@�9X@��;@���@��w@���@�|�@�K�@�
=@��R@���@���@���@��!@���@��+@�n�@�E�@�5?@�-@�$�@�$�@�$�@��@�{@�@���@��#@��-@�`B@��@���@���@�j@�Q�@�A�@� �@�  @��@��@��@��R@��\@�n�@�5?@��@�@��^@���@��7@��@�O�@�%@���@��j@��u@��D@��@�r�@�Z@�1'@�  @��m@��
@�|�@�t�@�t�@�dZ@�C�@�"�@�ȴ@�^5@��T@��7@�`B@�?}@��/@�z�@�b@�ƨ@�dZ@�33@���@���@���@�~�@�v�@�M�@�@���@��@�X@�7L@�&�@��@�Ĝ@�Q�@�1'@�(�@� �@���@�l�@�\)@�33@���@�=q@�5?@�J@���@�O�@�/@��@��`@���@�Ĝ@�Ĝ@���@��@���@�A�@��w@�;d@��!@��+@�v�@�ff@�V@�@�@��h@��h@���@�hs@�`B@��@��`@��9G�O�@�S�@|��@o�@fȴ@_|�@X��@S33@L��@E?}@=�@7�P@1�@';d@!��@9X@\)@"�@l�@ƨ@
M�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBB1BPB\BVBVBPBJBDB
=B	7B
=B	7B1B1B1B+B+B%BBBBBB��B�;B��B��B��B�\B�oB�JB�%B�Bz�Bv�Br�Bk�BhsBiyBhsBffB`BBT�BG�B;dB33B1'B-B)�B(�B&�B$�B�B�B\BB�B�mB�`B�NB�;B�)B�B��B��B�%Bx�BdZBVB>wB)�BuB��B�#B��BŢB��B�{B�%B�Bw�Bm�BdZBZBF�B.B%�B!�B�B
=B
�B
��B
ƨB
�LB
��B
��B
��B
�\B
{�B
^5B
@�B
'�B
hB
+B	��B	�B	�yB	�fB	�fB	�`B	�ZB	�BB	�B	ƨB	��B	�\B	�7B	�B	u�B	l�B	e`B	ZB	J�B	;dB	8RB	6FB	5?B	33B	1'B	.B	)�B	'�B	"�B	�B	�B	hB	
=B	B��B��B�B�fB�B��B��B��B��B��B��B��B��B��B��B��B��BȴBĜB��B��B��B�}B�}B�qB�XB�9B�!B�B��B��B��B��B��B��B�oB�VB�JB�=B�+B�B�B�B}�By�Bv�Bs�Bo�Bn�Bl�BjBhsBffBcTBbNB`BB_;B^5B]/B\)B[#BYBXBW
BVBVB<jBXBXBXBXBXBXBYBYBYBYBXBYBZB]/B_;B_;B`BB`BB`BB`BB`BB`BB`BBaHBcTBe`BgmBk�Bm�Bn�Bm�Bm�Bo�Bs�Bu�Bx�Bz�B|�B}�B�B�B�B�B�B�B�+B�7B�=B�PB�VB�\B�oB�{B��B�{B��B�hB�{B��B��B��B��B��B��B�3B�^B�}B��BBÖBÖBĜBĜBƨBȴB��B��B��B�B�/B�NB�ZB�`B�`B�fB�mB�mB�sB�sB�sB�mB�fB�`B�ZB�ZB�TB�NB�HB�NB�fB�B�B��B��B��B��B��B��B��B��B��B��B��B	B	%B	DB	bB	uB	�B	�B	�B	"�B	%�B	&�B	)�B	+B	,B	-B	/B	0!B	49B	9XB	=qB	>wB	>wB	?}B	@�B	A�B	E�B	I�B	M�B	P�B	R�B	VB	VB	W
B	XB	ZB	ZB	ZB	[#B	\)B	]/B	_;B	aHB	bNB	e`B	gmB	hsB	iyB	l�B	o�B	q�B	q�B	q�B	q�B	q�B	r�B	q�B	r�B	r�B	r�B	t�B	t�B	v�B	w�B	y�B	z�B	{�B	|�B	}�B	~�B	� B	�%B	�DB	�PB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�LB	�RB	�XB	�^B	�dB	�dB	�jB	�}B	��B	B	ÖB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�#B	�/B	�BB	�HB	�HB	�NB	�TB	�NB	�NB	�NB	�TB	�fB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
PB
�B
%�B
,B
1'B
7LB
?}B
E�B
I�B
L�B
P�B
W
B
]/B
cTB
jB
o�B
q�B
t�B
v�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B.BEBfBnBlBkBbB_BWB
SB	KB
SB	HBEBEBBB?B?B7B2B1B2B.B&B��B�NB��B�B��B�kB��B�^B�1B�Bz�Bv�Br�Bk�Bh�Bi�Bh�BfvB`RBUBG�B;rB3?B17B-B*	B)B&�B$�B�B�BjBB��B�{B�nB�\B�IB�9B�(B��B��B�2Bx�BdeBVB>�B*B�B��B�/B��BűB�B��B�1B�Bw�Bm�BdgBZ*BF�B."B%�B!�B�B
LB
�B
�B
ƷB
�\B
��B
��B
��B
�nB
{�B
^GB
@�B
(B
zB
BB	��B	�B	�B	�|B	�~B	�vB	�qB	�XB	�&B	ƿB	��B	�sB	�PB	�B	u�B	l�B	eyB	Z8B	J�B	;�B	8pB	6aB	5\B	3PB	1BB	.2B	*B	(B	"�B	�B	�B	�B	
YB	2B�B��B��B�B�3B��B�B�B�B�B�B�B�B�B�B�	B��B��BĿB��B��B��B��B��B��B�{B�[B�CB�'B�B� B��B��B��B��B��B�zB�pB�bB�PB�CB�8B�*B~BzBv�Bs�Bo�Bn�Bl�Bj�Bh�Bf�Bc{BbsB`iB_aB^[B]UB\OB[MBY=BX8BW2BV,BV,G�O�BX:BX:BX9BX7BX9BX6BY=BY=BY=BY=BX7BY>BZDB]VB_aB_aB`iB`lB`jB`jB`jB`hB`jBapBczBe�Bg�Bk�Bm�Bn�Bm�Bm�Bo�Bs�Bu�Bx�B{B}B~B�-B�1B�7B�7B�?B�DB�RB�ZB�`B�vB�yB�B��B��B��B��B��B��B��B��B��B��B��B��B�B�WB��B��B��B´BúBøB��B��B��B��B��B��B�B�6B�PB�pB�{B�B�B�B�B�B�B�B�B�B�B�B�|B�~B�uB�pB�kB�oB�B�B��B��B�B�B�B�B�B�B�B�B�B�B	:B	FB	dB	�B	�B	�B	�B	�B	"�B	&B	'B	*B	+B	,(B	-+B	/8B	0<B	4XB	9sB	=�B	>�B	>�B	?�B	@�B	A�B	E�B	I�B	M�B	QB	SB	V B	V B	W$B	X,B	Z9B	Z9B	Z<B	[@B	\FB	]KB	_YB	adB	blB	ezB	g�B	h�B	i�B	l�B	o�B	q�B	q�B	q�B	q�B	q�B	r�B	q�B	r�B	r�B	r�B	t�B	t�B	v�B	w�B	y�B	z�B	|B	}
B	~B	B	�B	�?B	�`B	�kB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�(B	�)B	�)B	�LB	�gB	�kB	�qB	�yB	�~B	�}B	��B	��B	��B	§B	îB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�/B	�7B	�6B	�=B	�<B	�<B	�FB	�[B	�bB	�`B	�eB	�kB	�eB	�dB	�cB	�lB	�}B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�G�O�B
 B
eB
�B
%�B
,B
1>B
7aB
?�B
E�B
I�B
L�B
P�B
W!B
]DB
cjB
j�B
o�B
q�B
t�B
v�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.13 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214332016053112143320160531121433  AO  ARCAADJP                                                                    20140721230534    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230534  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230534  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121433  IP                  G�O�G�O�G�O�                