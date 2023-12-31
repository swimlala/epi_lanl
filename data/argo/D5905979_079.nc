CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:12Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170912  20220204114418  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               OA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @���Ot}�1   @�����~@7�Q��c�(�\1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    OA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ��B��B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dq��Dry�Ds  Ds� Dt  Dt� Dt�3Dy�fD�)�D�`RD��=D��=D��D�]D���D��RD��D�P D���D��D�!�D�IHD�<{D���D�!HD�X�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���B G�BG�Bz�Bz�Bz�B'z�B/z�B7z�B?{BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��B��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C�RC޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU�RCW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce�RCg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)~D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DT~DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq�HDrqHDr��Dsw�Ds��Dtw�Dt��Dy�D�%�D�\)D��D��D�{D�X�D���D��)D��D�K�D���D���D�qD�ED�8RD���D�D�T�D�D�׮111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A͙�A͙�A͛�A͙�A͟�A͡�Aͣ�A͟�A͝�A͝�A͝�A�;dA�5?A��A���AǇ+A��Aç�A�Q�A��A�9XA�%A�oA��A�t�A��A���A�A�?}A�z�A��-A�E�A��A���A��#A��PA�x�A��hA�G�A�A�A�  A���A�&�A��HA���A�%A�|�A��A��A�^5A�bA���A���A��A�ȴA���A�XA�A���A��9A���A���A�9XA��TA���A��\A�ZA��\A�
=A�^5A� �A�ĜA��`A�n�A�S�A��A���A�S�A� �A�ffA�ZA�dZA�p�A��TA��A��A��FA��/A�K�A��;A���A�&�A��#A�ffA�JA�I�A�JA�\)A�ȴA�^5A�
=A�A��TA��A~9XA|��A{�mAz��Ay�^Av�jAt$�Ar��Aq�#Ao�PAn9XAk��Ah{Ag�PAg?}Af�Ad��Ab�jAb-AaAap�A`��A^�A[�mAX�AVv�AU�ARI�AP��AP1'AO�AOƨAO��AO��AO��AN�jALjAK�PAI��AG��AEXADZAB��AA��A?�A>$�A<ȴA:�A9|�A9�A9%A8�`A7�#A6��A6�A5VA3�A25?A1�A/�TA.ĜA,��A+��A*~�A*JA)|�A(M�A'x�A'7LA%��A#��A!t�A Q�A33A=qA  A��A?}A��AVA�9AA�A��A~�A33A��A�AjA�Al�Ar�A  A��A��AQ�A�-A�A+A
M�A	�;A	p�AA�A��A/A=qA��AhsA�A�mAO�A �A -@��@��@�dZ@���@��@���@�D@�t�@�{@�@�?}@�(�@�l�@��@��/@ꟾ@�A�@��@�%@�@� �@��@��@��@�@�v�@��@�j@�1@ߝ�@�;d@�@ް!@�7L@ڇ+@�dZ@�Q�@��@мj@�S�@ΰ!@Ͳ-@�`B@̋D@�\)@�~�@��@ɡ�@��
@�X@�A�@�"�@��-@��@��u@���@���@�~�@�ff@�J@��-@�V@���@��u@��/@�Ĝ@�I�@�=q@��^@�Q�@�t�@��@��@��\@�^5@�5?@�ff@���@�`B@�/@�p�@��h@���@��9@�(�@�n�@�V@�-@�`B@���@���@��h@��h@���@��;@��y@� �@�Q�@���@��P@�?}@���@�
=@�n�@��@��7@��H@�t�@�l�@�33@�ff@�O�@���@�z�@�Z@�1'@��;@��@��@��@�A�@� �@���@���@�\)@�
=@��!@��+@�E�@�?}@���@�1'@�C�@�^5@�5?@��@���@�l�@���@��w@���@�7L@���@��j@�A�@�K�@�~�@�V@�$�@��T@��^@���@���@��7@��7@�G�@��D@�ƨ@���@��P@��@�C�@�@��y@���@�v�@�^5@�M�@�$�@��@��#@��^@���@�x�@�7L@�V@��j@��@�r�@�I�@�9X@�9X@�b@��@��w@��@�l�@�C�@�
=@�ȴ@���@�V@�5?@�5?@�$�@�{@��T@�x�@�/@�V@���@�Ĝ@���@�j@� �@��
@�ƨ@��@��@�\)@�
=@��!@��+@�{@��h@�p�@�hs@�G�@��@�z�@�(�@��@��;@���@�t�@�l�@�dZ@�dZ@�S�@�+@��R@�V@���@��-@���@���@��-@�x�@�/@��@�Ĝ@�j@�A�@��@��w@�\)@�+@�o@�@���@���@��7@�7L@��@���@��@�j@�9X@� �@�b@�ƨ@�t�@�K�@��@��y@���@�n�@�V@�E�@��@���@��^@�#:@}��@sC@kx@b��@\<�@S��@J��@ET�@>d�@8�?@2^5@-��@(��@$<�@!5�@�@�@�7@�C@I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A͙�A͙�A͛�A͙�A͟�A͡�Aͣ�A͟�A͝�A͝�A͝�A�;dA�5?A��A���AǇ+A��Aç�A�Q�A��A�9XA�%A�oA��A�t�A��A���A�A�?}A�z�A��-A�E�A��A���A��#A��PA�x�A��hA�G�A�A�A�  A���A�&�A��HA���A�%A�|�A��A��A�^5A�bA���A���A��A�ȴA���A�XA�A���A��9A���A���A�9XA��TA���A��\A�ZA��\A�
=A�^5A� �A�ĜA��`A�n�A�S�A��A���A�S�A� �A�ffA�ZA�dZA�p�A��TA��A��A��FA��/A�K�A��;A���A�&�A��#A�ffA�JA�I�A�JA�\)A�ȴA�^5A�
=A�A��TA��A~9XA|��A{�mAz��Ay�^Av�jAt$�Ar��Aq�#Ao�PAn9XAk��Ah{Ag�PAg?}Af�Ad��Ab�jAb-AaAap�A`��A^�A[�mAX�AVv�AU�ARI�AP��AP1'AO�AOƨAO��AO��AO��AN�jALjAK�PAI��AG��AEXADZAB��AA��A?�A>$�A<ȴA:�A9|�A9�A9%A8�`A7�#A6��A6�A5VA3�A25?A1�A/�TA.ĜA,��A+��A*~�A*JA)|�A(M�A'x�A'7LA%��A#��A!t�A Q�A33A=qA  A��A?}A��AVA�9AA�A��A~�A33A��A�AjA�Al�Ar�A  A��A��AQ�A�-A�A+A
M�A	�;A	p�AA�A��A/A=qA��AhsA�A�mAO�A �A -@��@��@�dZ@���@��@���@�D@�t�@�{@�@�?}@�(�@�l�@��@��/@ꟾ@�A�@��@�%@�@� �@��@��@��@�@�v�@��@�j@�1@ߝ�@�;d@�@ް!@�7L@ڇ+@�dZ@�Q�@��@мj@�S�@ΰ!@Ͳ-@�`B@̋D@�\)@�~�@��@ɡ�@��
@�X@�A�@�"�@��-@��@��u@���@���@�~�@�ff@�J@��-@�V@���@��u@��/@�Ĝ@�I�@�=q@��^@�Q�@�t�@��@��@��\@�^5@�5?@�ff@���@�`B@�/@�p�@��h@���@��9@�(�@�n�@�V@�-@�`B@���@���@��h@��h@���@��;@��y@� �@�Q�@���@��P@�?}@���@�
=@�n�@��@��7@��H@�t�@�l�@�33@�ff@�O�@���@�z�@�Z@�1'@��;@��@��@��@�A�@� �@���@���@�\)@�
=@��!@��+@�E�@�?}@���@�1'@�C�@�^5@�5?@��@���@�l�@���@��w@���@�7L@���@��j@�A�@�K�@�~�@�V@�$�@��T@��^@���@���@��7@��7@�G�@��D@�ƨ@���@��P@��@�C�@�@��y@���@�v�@�^5@�M�@�$�@��@��#@��^@���@�x�@�7L@�V@��j@��@�r�@�I�@�9X@�9X@�b@��@��w@��@�l�@�C�@�
=@�ȴ@���@�V@�5?@�5?@�$�@�{@��T@�x�@�/@�V@���@�Ĝ@���@�j@� �@��
@�ƨ@��@��@�\)@�
=@��!@��+@�{@��h@�p�@�hs@�G�@��@�z�@�(�@��@��;@���@�t�@�l�@�dZ@�dZ@�S�@�+@��R@�V@���@��-@���@���@��-@�x�@�/@��@�Ĝ@�j@�A�@��@��w@�\)@�+@�o@�@���@���@��7@�7L@��@���@��@�j@�9X@� �@�b@�ƨ@�t�@�K�@��@��y@���@�n�@�V@�E�@��@���G�O�@�#:@}��@sC@kx@b��@\<�@S��@J��@ET�@>d�@8�?@2^5@-��@(��@$<�@!5�@�@�@�7@�C@I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�oB��B�`BoBH�B{�B��B�B�-B�FB�qB�?B�B�5B��BBB��B��B  B��B��B��B��B�NB��B�}B�wB��BBĜBȴBƨBƨBŢB��B��B��B��B��B��B��B��BɺB��B��BȴBŢBÖB�}B�?B�!B��B��B��B�{B�JBv�B>wB,B(�B"�B�B�B�B �B"�B#�B!�BoB��B��B�PB�VB�DB�Bn�B]/BQ�B8RB	7B
�B
�B
ŢB
�'B
��B
��B
�hB
�+B
� B
p�B
_;B
M�B
?}B
0!B
%�B
�B
�B
DB	��B	�sB	�/B	�B	ǮB	�XB	�B	�hB	�JB	�=B	�+B	z�B	l�B	hsB	e`B	bNB	^5B	Q�B	?}B	-B	�B	�B	DB��B��B��B��B��B��B��B�B�TB�;B�B��BǮBƨB��B�dB�FB�B��B��B��B��B��B��B��B��B��B��B��B��B�DB�1B�Bx�Br�Bp�Bo�Bn�BjBhsBiyBl�BbNB[#B]/BYBT�BS�BQ�BP�BN�BO�BN�BL�BL�BK�BL�BL�BM�BO�BQ�BVB\)B[#BZBXBT�BS�BQ�BN�BN�BM�BL�BJ�BG�BF�BD�BE�BB�B@�B?}B>wB<jB<jB:^B:^B;dB9XB<jB<jB>wB>wB?}B>wB>wB?}B?}B@�B@�BC�BE�BG�BG�BG�BH�BH�BH�BH�BI�BK�BM�BM�BL�BL�BL�BL�BK�BM�BR�BZB\)B]/B]/B\)B[#B]/B]/B]/B]/B\)B[#BZB\)BbNBdZBgmBjBk�Bn�Bp�Bo�Bp�Bo�Bo�Bq�Br�Bs�Bv�B}�B�%B�=B�7B�1B�DB�=B�DB�JB�JB�PB�bB��B��B��B��B��B�B�B�B�'B�9B�LB�dB�qBBÖBĜBȴBɺBɺB��B�B�/B�BB�NB�B	B		7B		7B	
=B		7B	oB	�B	�B	�B	 �B	#�B	#�B	%�B	'�B	)�B	,B	/B	1'B	33B	8RB	:^B	:^B	:^B	:^B	9XB	9XB	=qB	?}B	C�B	E�B	D�B	B�B	B�B	C�B	E�B	C�B	>wB	I�B	H�B	D�B	C�B	D�B	E�B	F�B	F�B	G�B	J�B	J�B	K�B	K�B	L�B	L�B	L�B	L�B	M�B	P�B	S�B	S�B	T�B	T�B	W
B	XB	XB	YB	ZB	[#B	\)B	]/B	`BB	aHB	cTB	dZB	dZB	e`B	ffB	jB	k�B	l�B	n�B	q�B	r�B	v�B	w�B	y�B	{�B	|�B	~�B	�B	�B	�%B	�1B	�7B	�7B	�=B	�DB	�JB	�\B	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�!B	�'B	�-B	�-B	�3B	�9B	�FB	�RB	�XB	�XB	�^B	�dB	�}B	B	ÖB	ĜB	ĜB	ĜB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�B	�B
�B
jB
�B
#nB
-CB
5�B
<6B
CGB
G_B
N�B
R�B
UMB
ZkB
]B
dZB
h$B
m)B
q'B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B�?BݿB
�BABt>B��B�WB��B��B��B��B�]BֈB�KB�cB�jB�KB�@B�RB�3B�9B�LB�9BڢB�B��B��B��B��B��B�
B��B��B��B�B�B�#B�)B�/B�)B�)B�*B�B�0B�*B�B��B��B��B��B�zB�=B�B��B��B��Bo&B6�B$jB!XB3BB�BB(B4B:B.B
�B�4B�AB��B��B��B{BgBU�BJ]B0�B�B
�B
ҖB
�B
��B
�AB
�)B
��B
�B
xB
i$B
W�B
FUB
8 B
(�B
hB
2B
B
�B	�oB	��B	չB	ΏB	�:B	��B	��B	��B	��B	��B	�B	srB	eB	aB	]�B	Z�B	V�B	J�B	8B	%�B	PB	8B	�B��B�{B�oB�oB�jB�dB�^B�QB��B��BҺBɃB�LB�GB�"B�B��B��B��B�~B�ZB�ZB�ZB�TB�gB�NB�gB�ZB�6B�$B��B��Bz�Bq{BkVBiJBhDBg>Bc&BaBb Be2BZ�BS�BU�BQ�BM�BL�BJ�BI�BG�BH�BG�BEwBEwBDrBExBExBF~BH�BJ�BN�BT�BS�BR�BP�BM�BL�BJ�BG�BG�BFBEyBCmB@[B?UB=IB>OB;<B91B8+B7%B5B5B3B3B4B2B5B5B7&B7&B8,B7&B7&B8,B8,B92B92B<EB>QB@]B@]B@]BAcBAcBAcBAcBBiBDvBF�BF�BE|BE|BE|BE|BDvBF�BK�BR�BT�BU�BU�BT�BS�BU�BU�BU�BU�BT�BS�BR�BT�BZ�B]
B`Bc.Bd4BgGBiSBhMBiSBhMBhMBjYBk_BleBoxBv�B~�B��B��B��B��B��B��B��B��B��B�B�YB��B��B��B��B��B��B��B��B��B��B�B�B�;B�AB�GB�_B�eB�eB�xBίB��B��B��B�4B��B	�B	�B	�B	�B	B	FB	RB	XB	kB	}B	}B	�B	 �B	"�B	$�B	'�B	)�B	+�B	0�B	3B	3B	3B	3B	1�B	1�B	6B	8"B	<;B	>GB	=AB	;4B	;4B	<;B	>GB	<;B	7B	B_B	AYB	=AB	<;B	=AB	>GB	?MB	?MB	@SB	CfB	CfB	DlB	DlB	ErB	ErB	ErB	ErB	FxB	I�B	L�B	L�B	M�B	M�B	O�B	P�B	P�B	Q�B	R�B	S�B	T�B	U�B	X�B	Y�B	[�B	\�B	\�B	^B	_
B	c#B	d)B	e/B	g;B	jMB	kSB	olB	prB	r~B	t�B	u�B	w�B	y�B	|�B	~�B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�"B	�(B	�4B	�AB	�AB	�GB	�MB	�SB	�_B	�_B	�_B	�lB	�lB	�lB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�/B	�5B	�;B	�;B	�;B	�AB	�MB	�SB	�YB	�`B	�rB	�xB	�~B	ʋB	ʋB	ˑB	̗B	͝B	ЯB	һB	һB	ҼB	��B	��B	��B	��B	��G�O�B	ۣB	�NB	�{B
B
(B

B
%�B
.xB
4�B
;�B
?�B
G@B
KXB
M�B
SB
U�B
\�B
`�B
e�B
i�B
pN111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.13 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144182022020411441820220204114418  AO  ARCAADJP                                                                    20200619170912    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170912  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170912  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114418  IP                  G�O�G�O�G�O�                