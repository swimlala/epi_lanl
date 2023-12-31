CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-03-24T07:01:08Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20180324070108  20190604094145  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�U�]L?O1   @�UɔJZX@5PbM���d�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @9��@�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D��D�K3D�~�D��HD�3D�2�D�w\D���D��D�?
D���D�ҏD��D�I�D�eqD���D� D�@ D�RD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @1G�@w�@��
@��
AQ�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��>B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB��B۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt�{Dy�fD��fD�G
D�z�D��D�
D�.fD�s3D�њD�{D�:�D�\D��fD��D�E�D�aHD�ϮD��D�;�D�)D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��A��A��A��A�Aġ�AăA�p�A�ffA�\)A�"�A�oA�VA�%A��;Aú^Aß�AÏ\A�`BA��#A�l�A���A���A�|�A�A�A�9XA�/A��FA�9XA�(�A�
=A�  A��yA���A���A�I�A�1A��A��7A�t�A�A�=qA���A�x�A�9XA�1A��^A��A�33A���A��jA��jA��A�+A���A��yA�(�A�/A��yA�9XA��9A�n�A���A���A�K�A��A�~�A�p�A��#A�/A���A�p�A�M�A� �A�33A��hA���A�7LA��uA��A�Q�A�33A�A��FA�ZA�n�A���A�z�A��
A��A�  A��A��A�I�A�M�A��A���A�bNA��HA�|�A�C�A��
A��A�  A��A~1AzI�AsS�ApM�Ao�An�\Al�!Ak�AiC�Agp�Ab�HAa��A`�A_%A^=qA]�A]"�A\ZAZ�/AX�AV�AQ�AO�PAM�#ALZAK
=AJĜAI�7AI33AH=qAG;dAFffAE��AD��AC`BAB=qAA��A@��A?�TA>ffA=��A;|�A9�^A8z�A7��A7\)A5�
A4�A3�;A3�A0�A.r�A-A,�A,  A*^5A)�
A(�9A%S�A"��A"^5A"I�A"�A!C�A jA�A5?A;dA�A��A&�A�A^5AE�AAƨA�^A�-A;dA�#A\)AVA�\A��A��AbAVA\)A+AA�hA&�AM�AA�A�hA�A&�A
�DA	�;A	t�A�A1'A;dA�!A�
AbA�AA�A7L@��@��w@���@�{@�Q�@�
=@�@�\)@�J@�1'@�&�@�^@�S�@��H@�~�@�$�@�p�@��@�z�@�9X@���@�|�@��@�v�@�-@ݩ�@�/@ܣ�@��H@�{@���@١�@��@֟�@�p�@�I�@��m@ӥ�@�o@�J@�G�@���@Ѓ@��m@�"�@�V@ͩ�@�Q�@�S�@�E�@���@�7L@�1@�o@�O�@�A�@���@�
=@§�@+@���@��@��j@�(�@���@�C�@�M�@�&�@�1'@�(�@�1@��w@�33@���@�@�x�@��;@�K�@�C�@��!@�$�@�`B@���@�j@��
@�|�@���@�@�G�@��@�9X@�l�@�C�@�V@��#@��-@�O�@��@���@�(�@���@�l�@�33@��@��y@�~�@�5?@�$�@�@��@��
@���@�{@��T@�x�@���@���@�z�@�1@���@�\)@�K�@�v�@��^@�&�@���@���@�b@�Q�@��@���@��/@��j@��u@�z�@�(�@� �@���@��m@��
@��@��P@�\)@�S�@�dZ@�\)@�\)@�@���@�v�@�5?@�{@���@��7@�hs@�X@���@�bN@��@��@���@�t�@�dZ@�
=@��@�V@��h@���@��D@�I�@�1@��;@�ƨ@��w@���@�l�@�;d@��@��R@�^5@�@��#@�@�@���@�O�@���@���@��D@��@�1@��@���@��@�\)@�o@��@��@�ȴ@���@�v�@�^5@�{@��T@��-@�x�@�G�@��/@��@�r�@�A�@��;@���@��P@�"�@��R@�E�@�$�@��@���@��h@�G�@��@���@���@���@�bN@� �@��m@�ƨ@��F@��@�l�@�K�@�o@��y@���@�v�@�^5@�=q@�$�@��T@�p�@�?}@�?}@�Ĝ@�Z@� �@�b@�  @���@�t�@��@��H@���@���@�-@���@��h@���@��h@�p�@���@��D@�z�@�(�@��;@��w@���@�;d@��@�W@yV@q��@m��@g"�@]��@W�w@O/�@KO@E�X@=��@4�/@.�6@(oi@"��@z@k�@��@]d@\)@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A��A��A��A��A��A��A��A��A�Aġ�AăA�p�A�ffA�\)A�"�A�oA�VA�%A��;Aú^Aß�AÏ\A�`BA��#A�l�A���A���A�|�A�A�A�9XA�/A��FA�9XA�(�A�
=A�  A��yA���A���A�I�A�1A��A��7A�t�A�A�=qA���A�x�A�9XA�1A��^A��A�33A���A��jA��jA��A�+A���A��yA�(�A�/A��yA�9XA��9A�n�A���A���A�K�A��A�~�A�p�A��#A�/A���A�p�A�M�A� �A�33A��hA���A�7LA��uA��A�Q�A�33A�A��FA�ZA�n�A���A�z�A��
A��A�  A��A��A�I�A�M�A��A���A�bNA��HA�|�A�C�A��
A��A�  A��A~1AzI�AsS�ApM�Ao�An�\Al�!Ak�AiC�Agp�Ab�HAa��A`�A_%A^=qA]�A]"�A\ZAZ�/AX�AV�AQ�AO�PAM�#ALZAK
=AJĜAI�7AI33AH=qAG;dAFffAE��AD��AC`BAB=qAA��A@��A?�TA>ffA=��A;|�A9�^A8z�A7��A7\)A5�
A4�A3�;A3�A0�A.r�A-A,�A,  A*^5A)�
A(�9A%S�A"��A"^5A"I�A"�A!C�A jA�A5?A;dA�A��A&�A�A^5AE�AAƨA�^A�-A;dA�#A\)AVA�\A��A��AbAVA\)A+AA�hA&�AM�AA�A�hA�A&�A
�DA	�;A	t�A�A1'A;dA�!A�
AbA�AA�A7L@��@��w@���@�{@�Q�@�
=@�@�\)@�J@�1'@�&�@�^@�S�@��H@�~�@�$�@�p�@��@�z�@�9X@���@�|�@��@�v�@�-@ݩ�@�/@ܣ�@��H@�{@���@١�@��@֟�@�p�@�I�@��m@ӥ�@�o@�J@�G�@���@Ѓ@��m@�"�@�V@ͩ�@�Q�@�S�@�E�@���@�7L@�1@�o@�O�@�A�@���@�
=@§�@+@���@��@��j@�(�@���@�C�@�M�@�&�@�1'@�(�@�1@��w@�33@���@�@�x�@��;@�K�@�C�@��!@�$�@�`B@���@�j@��
@�|�@���@�@�G�@��@�9X@�l�@�C�@�V@��#@��-@�O�@��@���@�(�@���@�l�@�33@��@��y@�~�@�5?@�$�@�@��@��
@���@�{@��T@�x�@���@���@�z�@�1@���@�\)@�K�@�v�@��^@�&�@���@���@�b@�Q�@��@���@��/@��j@��u@�z�@�(�@� �@���@��m@��
@��@��P@�\)@�S�@�dZ@�\)@�\)@�@���@�v�@�5?@�{@���@��7@�hs@�X@���@�bN@��@��@���@�t�@�dZ@�
=@��@�V@��h@���@��D@�I�@�1@��;@�ƨ@��w@���@�l�@�;d@��@��R@�^5@�@��#@�@�@���@�O�@���@���@��D@��@�1@��@���@��@�\)@�o@��@��@�ȴ@���@�v�@�^5@�{@��T@��-@�x�@�G�@��/@��@�r�@�A�@��;@���@��P@�"�@��R@�E�@�$�@��@���@��h@�G�@��@���@���@���@�bN@� �@��m@�ƨ@��F@��@�l�@�K�@�o@��y@���@�v�@�^5@�=q@�$�@��T@�p�@�?}@�?}@�Ĝ@�Z@� �@�b@�  @���@�t�@��@��H@���@���@�-@���@��h@���@��h@�p�@���@��D@�z�@�(�@��;@��w@���@�;dG�O�@�W@yV@q��@m��@g"�@]��@W�w@O/�@KO@E�X@=��@4�/@.�6@(oi@"��@z@k�@��@]d@\)@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBVBVBVBVBW
BVBVBVBT�BT�BVBW
BXBYB[#B[#B[#B[#B\)B^5BdZBl�B{�B��B�BȴB�5B��B	7BPBoB7LBM�B[#BcTBp�Bs�Bu�B{�B~�B�B�B�%B�%B�=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�\B�=B�7B�1Bx�BS�BM�BI�B@�B<jBC�BD�B?}B7LB%�B�B��B�B�;B�B��B�}B��B��B��B�uB�hB�PB�+By�BgmB`BB[#BP�BB�B49B�BB
��B
�B
�B
�yB
�NB
�B
ĜB
��B
�B
ffB
W
B
D�B
%�B	��B	�`B	�/B	�
B	��B	�}B	�3B	��B	�=B	�B	y�B	s�B	o�B	l�B	hsB	cTB	\)B	P�B	C�B	.B	!�B	�B	uB	PB	JB		7B	%B	B��B��B��B�B�yB�ZB�HB�/B�B��B��BǮB��B�wB�jB�XB�?B�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�\B�JB�=B�7B�1B�%B�B�B�B�B�B�B�B~�B}�B|�B{�By�Bx�Bv�Bt�Br�Bq�Bp�Bo�Bn�Bm�Bm�Bl�Bk�BjBiyBiyBhsBhsBgmBgmBffBffBe`BdZBe`BffBe`BdZBcTBaHBaHB_;B_;B_;B_;B]/B[#BZBZBZB]/B^5B^5B^5B_;B_;BaHBaHBbNBbNBdZBe`Be`BffBgmBhsBm�Bn�Bo�Bn�Bo�Bs�Bt�Bu�Bv�Bv�Bz�B|�B}�B�B�B�B�B�B�%B�7B�DB�\B�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�?B�?B�FB�RB�^B�dB�jB��BŢBɺB��B��B��B��B�
B�B�#B�/B�;B�NB�fB�fB�mB�B�B��B��B��B��B��B	B	B		7B	DB	PB	PB	\B	oB	{B	{B	�B	�B	�B	 �B	$�B	$�B	(�B	+B	,B	-B	1'B	33B	5?B	:^B	=qB	?}B	C�B	E�B	K�B	K�B	P�B	YB	[#B	]/B	_;B	`BB	aHB	bNB	dZB	e`B	ffB	ffB	hsB	jB	m�B	n�B	q�B	t�B	v�B	y�B	|�B	�B	�B	�B	�%B	�7B	�=B	�=B	�JB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�?B	�XB	�dB	�jB	�jB	�qB	��B	B	ÖB	ŢB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�/B	�/B	�5B	�BB	�NB	�NB	�TB	�`B	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
	7B
1B
	7B
	7B
	7B
	7B

=B
JB
�B
B
 BB
(�B
0�B
:*B
=�B
A�B
J	B
M�B
U�B
\�B
c�B
iDB
m�B
p�B
u�B
z�B
�B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 BF�BF�BF�BF�BG�BF�BF�BF�BE�BE�BF�BG�BH�BI�BK�BK�BK�BK�BL�BN�BUB]ABl�B�>B��B�bB��B�iB��B��BB'�B>pBK�BS�Ba9BdNBfaBl|Bo�Bq�Bu�Bv�Bv�Bz�B�(B�@B�TB�^B�dB�fB�_B�XB�UB�SB�FB�IB�8B�$B�B�Bz�By�Bx�BisBD�B>vB:]B1$B-B47B5<B0B'�B�B	?B�B�GB��B��B��B�4B��B�KB�:B�3B�$B~Bw�Bj�BX-BQBK�BA�B3UB$�B�B
��B
�B
�vB
�]B
�KB
�#B
��B
�vB
��B
s�B
WNB
G�B
5�B
�B	��B	�ZB	�)B	�B	��B	�}B	�2B	��B	{DB	uB	j�B	d�B	`�B	]�B	Y~B	T\B	M9B	A�B	4�B	(B	�B	
�B	�B�hB�dB�PB�;B�B�B��B��BߴBړB�uB�fB�QB�/B�B��B��B��B��B��B��B�dB�RB�BB�2B�B� B�B�B�	B�B��B��B��B��B��B��B��B��B��B��B}zB{nBzgByeBwUBvRBuIBtGBtABs=Bs>Br:Bp)Bo)BnBmBkBjBg�Be�Bc�Bb�Ba�B`�B_�B^�B^�B]�B\�B[�BZ�BZ�BY�BY�BX�BX�BW�BW�BV�BU�BV�BW�BV�BU�BT�BR�BR�BPvBPxBPwBPyBNnBLdBKZBKYBK[BNnBOvBOrBOvBP~BPyBR�BR�BS�BS�BU�BV�BV�BW�BX�BY�B^�B_�B`�B_�B`�Bd�Be�BgBhBhBlBn-Bo0BrBBrCBsFBuXBv]BwcBzsB|�B��B��B��B��B��B��B��B��B��B��B��B��B�B�&B�1B�9B�?B�FB�bB�yB�yB�~B��B��B��B��B��B��B��B��B�
B�
B�%B�?B�KB�YB�dB�sBӂBךBכBأB��B��B��B�	B�B�'B�,B�:B�JB�lB�sB��B�B	 �B	�B	�B	�B	�B	�B	�B	�B	B	B	&B	3B	5B	>B	"TB	$eB	&lB	+�B	.�B	0�B	4�B	6�B	<�B	<�B	BB	J?B	LOB	N[B	PdB	QnB	RnB	SxB	UB	V�B	W�B	W�B	Y�B	[�B	^�B	_�B	b�B	e�B	g�B	k B	nB	r-B	u>B	vEB	wNB	zYB	{_B	{bB	}oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�)B	�/B	�/B	�0B	�5B	�EB	�NB	�UB	�^B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	�B	�B	�B	�B	�!B	�)B	�*B	�3B	�9B	�@B	�OB	�LB	�QB	�\B	�hB	�hB	�nB	�yB	ُB	ڕB	ړB	ۙB	ܞB	ݪB	ޭB	߳B	ߴB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�&B	�$B	�/B	�0B	�2B	�5B	�0B	�1B	�4B	�>B	�<B	�FB	�KB	�JB	�GB	�NB	�JB	�NB	�PB	�PB	�OB	�YG�O�B	�B
B
[B
�B
!�B
+@B
.�B
2�B
;"B
>�B
F�B
NB
T�B
ZWB
^�B
a�B
f�B
k�B
p�B
t�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.13 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.015(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040941452019060409414520190604094145  AO  ARCAADJP                                                                    20180324070108    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180324070108  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180324070108  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094145  IP                  G�O�G�O�G�O�                