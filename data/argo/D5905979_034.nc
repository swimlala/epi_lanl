CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:13:45Z creation      
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
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200618141345  20220204114413  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               "A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ؐ�co1   @ؐ\�3@8��$�/�c���+1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    "A   B   B   @���@�  A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C�C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D�fDfD�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-y�D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy��D�3D�Z�D��HD�ÅD�fD�R�D��
D��D�#3D�YHD��fD�߮D��D�VfDژ�D��HD�
D�S�D�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�p�@��
@��
A�A=�A]�A}�A�A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��>B��>B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB��B۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C�RC�RC�RC޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D�HD	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D�HDw�D��Dw�D��Dw�D��Dw�D��D~D�D~D�Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-qHD-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��DtqHDy��D�
D�VfD��D��\D�=D�NfD���D���D�
D�UD��=D�ۅD��D�R=Dڔ{D��D��D�O\D��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��
A��
A��
A���A���A��/A��/A��;A��;A��HA��TA��`A��mA��yA��A��yA��A��yA��mA��mA��yA��yA��yA��A��A��A��A��A��A���A���A���A���A��A��AϼjA�&�A�x�Aś�AĴ9A�VA��A�oA��A�XA���A�Q�A�"�A��FA�jA��`A��+A���A�bA�"�A��#A��+A�33A��
A���A��`A�ffA���A���A�JA�/A�I�A��wA� �A���A��RA���A��A�hsA��A�ȴA�n�A�+A���A��yA�1'A�C�A��A�ȴA�~�A��+A���A��A�&�A���A�ĜA�C�A�hsA���A�p�A��
A��A�~�A�z�A�I�A�{A�n�A���A�S�A�hsA�XA�XA�G�A��A�ffA��uA�XA��A�
=A�
=A��FA���A�bNA~n�A|z�Axn�At�ArVAq+Ap�/Ap�!Ap�uApQ�Ao�Ao&�Am��Am&�Ak"�AiO�Af��Af  Ae�wAd�RAc�
Aa��AaA`M�A_7LA]��A]
=AZĜAV-AU/AT1'AS\)AR��AM��AL1AJ�uAI�wAI�AH��AG7LADVAB�jA@1A>�A=`BA<��A;K�A:�HA9�mA8~�A6ȴA6{A4�9A4E�A2��A1��A0�HA/��A-l�A)��A)S�A(=qA(bA'�#A';dA&��A&Q�A&  A%��A%hsA$�A#��A#VA"n�A"E�A!��A!p�A!?}A ��A��A��A5?A��A�AI�AhsAE�A�A��A �A�AJA�jA�AXA-A
=AS�A
�A
VA	�mA	`BA�uA%A~�AdZAv�A�TAG�A �A�FA �!@�K�@�$�@���@�ȴ@�%@�|�@��+@���@���@�b@���@��@�
=@�9X@��
@�o@�@�V@��/@�F@��@�O�@�\)@�{@���@��#@�x�@��@�K�@�^5@�@�/@ܴ9@�33@���@ٙ�@��@�K�@��H@֧�@��@ԣ�@Ұ!@�M�@�1'@���@�$�@�p�@�Z@�\)@�33@���@ʟ�@�5?@���@�j@�t�@�ff@ź^@š�@�O�@���@�
=@�~�@��^@�X@��`@��m@��@�@��D@���@�"�@�{@�/@�dZ@�$�@�&�@��u@�z�@�Z@� �@���@��w@�K�@��R@�M�@��^@��h@�hs@���@��D@�Z@� �@�l�@�
=@��\@�$�@�x�@�%@��D@�@��T@�O�@��u@�1@���@���@���@�ƨ@��;@��;@��
@�l�@�p�@�&�@�?}@���@���@�`B@�?}@��j@���@��j@�9X@��@�;d@�dZ@�dZ@���@�j@���@�p�@��T@�-@�{@��T@�=q@��@��h@��7@�`B@�V@���@��`@���@�%@���@��@�j@�(�@���@��T@�J@��+@�@�
=@��\@�Ĝ@���@�/@��`@���@�bN@��F@�;d@�ff@��-@��7@�7L@��u@��j@���@�j@�I�@� �@�b@���@�  @��@�Ĝ@��@��`@�9X@��@���@�33@��@�o@���@��\@�n�@�^5@��#@��7@�x�@��@���@���@��@�V@��/@�Ĝ@��@��F@��@���@��;@��@�"�@��+@��@��`@���@�p�@��7@��@��/@��@��@��D@�bN@�ƨ@��@��@�n�@��@��-@��@�X@�G�@���@���@���@��@��D@�bN@�Q�@�I�@�Q�@�z�@�bN@�Q�@�1@��
@�"�@���@�n�@�$�@�@��@��@��-@�p�@�X@�7L@��@���@��$@}ԕ@u�@ll"@e�T@^�r@T�@O�@H��@Bں@;�K@5`B@/X�@*s�@%��@4�@L0@p�@��@=�@	N<11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��
A��
A��
A���A���A��/A��/A��;A��;A��HA��TA��`A��mA��yA��A��yA��A��yA��mA��mA��yA��yA��yA��A��A��A��A��A��A���A���A���A���A��A��AϼjA�&�A�x�Aś�AĴ9A�VA��A�oA��A�XA���A�Q�A�"�A��FA�jA��`A��+A���A�bA�"�A��#A��+A�33A��
A���A��`A�ffA���A���A�JA�/A�I�A��wA� �A���A��RA���A��A�hsA��A�ȴA�n�A�+A���A��yA�1'A�C�A��A�ȴA�~�A��+A���A��A�&�A���A�ĜA�C�A�hsA���A�p�A��
A��A�~�A�z�A�I�A�{A�n�A���A�S�A�hsA�XA�XA�G�A��A�ffA��uA�XA��A�
=A�
=A��FA���A�bNA~n�A|z�Axn�At�ArVAq+Ap�/Ap�!Ap�uApQ�Ao�Ao&�Am��Am&�Ak"�AiO�Af��Af  Ae�wAd�RAc�
Aa��AaA`M�A_7LA]��A]
=AZĜAV-AU/AT1'AS\)AR��AM��AL1AJ�uAI�wAI�AH��AG7LADVAB�jA@1A>�A=`BA<��A;K�A:�HA9�mA8~�A6ȴA6{A4�9A4E�A2��A1��A0�HA/��A-l�A)��A)S�A(=qA(bA'�#A';dA&��A&Q�A&  A%��A%hsA$�A#��A#VA"n�A"E�A!��A!p�A!?}A ��A��A��A5?A��A�AI�AhsAE�A�A��A �A�AJA�jA�AXA-A
=AS�A
�A
VA	�mA	`BA�uA%A~�AdZAv�A�TAG�A �A�FA �!@�K�@�$�@���@�ȴ@�%@�|�@��+@���@���@�b@���@��@�
=@�9X@��
@�o@�@�V@��/@�F@��@�O�@�\)@�{@���@��#@�x�@��@�K�@�^5@�@�/@ܴ9@�33@���@ٙ�@��@�K�@��H@֧�@��@ԣ�@Ұ!@�M�@�1'@���@�$�@�p�@�Z@�\)@�33@���@ʟ�@�5?@���@�j@�t�@�ff@ź^@š�@�O�@���@�
=@�~�@��^@�X@��`@��m@��@�@��D@���@�"�@�{@�/@�dZ@�$�@�&�@��u@�z�@�Z@� �@���@��w@�K�@��R@�M�@��^@��h@�hs@���@��D@�Z@� �@�l�@�
=@��\@�$�@�x�@�%@��D@�@��T@�O�@��u@�1@���@���@���@�ƨ@��;@��;@��
@�l�@�p�@�&�@�?}@���@���@�`B@�?}@��j@���@��j@�9X@��@�;d@�dZ@�dZ@���@�j@���@�p�@��T@�-@�{@��T@�=q@��@��h@��7@�`B@�V@���@��`@���@�%@���@��@�j@�(�@���@��T@�J@��+@�@�
=@��\@�Ĝ@���@�/@��`@���@�bN@��F@�;d@�ff@��-@��7@�7L@��u@��j@���@�j@�I�@� �@�b@���@�  @��@�Ĝ@��@��`@�9X@��@���@�33@��@�o@���@��\@�n�@�^5@��#@��7@�x�@��@���@���@��@�V@��/@�Ĝ@��@��F@��@���@��;@��@�"�@��+@��@��`@���@�p�@��7@��@��/@��@��@��D@�bN@�ƨ@��@��@�n�@��@��-@��@�X@�G�@���@���@���@��@��D@�bN@�Q�@�I�@�Q�@�z�@�bN@�Q�@�1@��
@�"�@���@�n�@�$�@�@��@��@��-@�p�@�X@�7L@��G�O�@��$@}ԕ@u�@ll"@e�T@^�r@T�@O�@H��@Bں@;�K@5`B@/X�@*s�@%��@4�@L0@p�@��@=�@	N<11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B(�BH�BE�BB�BM�BP�BXBXBcTBiyBgmBp�Bo�Bn�Bp�Br�Bs�Bp�Bq�Bq�Bq�Br�Bt�Bt�Bw�Bx�Bw�Bx�Bx�B{�B|�B~�B�B�B�B�B~�B}�B}�B|�B{�B|�Bw�Br�BhsB[#BD�B2-B�B	7B�B�mB�NB�B��BÖB�XB�-B�B��B��B�BgmBN�B@�B%�B�B�B\B
��B
�B
�;B
��B
��B
�FB
��B
��B
��B
�{B
�VB
� B
hsB
W
B
A�B
.B
JB	��B	�B	�B	�B	�B	�B	�yB	�ZB	�5B	�
B	��B	�}B	�!B	��B	��B	��B	��B	�oB	�VB	�PB	�1B	{�B	t�B	k�B	O�B	E�B	<jB	33B	-B	'�B	�B	DB	B��B��B��B�B�B�B��BȴBĜB�jB�LB�?B�FB�B�B��B��B��B��B��B�VB~�Bn�BffBhsBiyBhsBffBe`BcTBbNB`BB`BBbNBhsBe`BbNB`BB^5B\)B[#BZBYBW
BVBQ�BQ�BO�BO�BN�BM�BK�BI�BG�BD�B?}B=qB;dB5?B8RB49B1'B/B/B-B.B,B+B.B+B)�B)�B)�B(�B,B)�B)�B)�B,B)�B)�B)�B)�B-B,B/B,B-B/B.B-B-B+B+B-B2-B1'B5?B8RB8RB8RB9XB<jB=qB?}B@�B@�B@�BD�BE�BE�BH�BG�BH�BH�BI�BI�BK�BK�BP�BO�BO�BO�BP�BR�BR�BR�BR�BS�BS�BYBYB]/B]/B]/B]/BbNBdZBe`BhsBhsBiyBl�Bn�Bp�Bu�Bv�Bv�Bz�B|�B�B�1B�JB�PB�PB�VB�VB�VB�\B�bB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�9B�FB�XB��BĜBŢBɺB��B��B��B��B�B�B�)B�NB�ZB�yB��B��B��B	B	B	B	B		7B	VB	hB	
=B	\B	�B	�B	�B	�B	�B	$�B	&�B	%�B	%�B	)�B	-B	0!B	49B	49B	5?B	6FB	7LB	6FB	5?B	33B	0!B	2-B	7LB	;dB	A�B	E�B	A�B	E�B	J�B	P�B	S�B	T�B	VB	W
B	YB	YB	YB	YB	ZB	^5B	_;B	bNB	bNB	bNB	bNB	dZB	e`B	gmB	cTB	bNB	iyB	k�B	l�B	m�B	o�B	u�B	y�B	x�B	w�B	w�B	y�B	z�B	{�B	|�B	�B	�B	�B	�%B	�+B	�=B	�PB	�\B	�bB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�LB	�XB	�dB	�jB	�jB	�wB	��B	��B	B	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�;B	�>B
�B

�B
�B
�B
'B
+�B
2aB
9�B
BB
I�B
K^B
Q4B
XEB
]�B
abB
gB
o B
q�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B�6B�0B�0B�6B�0B�0B�0B�0B�0B�0B�0B�0B�0B�0B�0B�0B�0B�0B�0B�0B�0B�0B�0B�0B�6B�6B�6B�BB�HB�BB�BB�BB�6B�6B�0B�6B ZB@B=B9�BE7BHIBOtBOtBZ�B`�B^�BhBgBe�BhBjBkBhBiBiBiBjBl Bl Bo3Bp9Bo4Bp:Bp:BsLBtSBv_ByqByqByqBxkBv_BuYBuYBtSBsMBtSBo5BjB_�BR�B<B)�B%B �B��B��B��BЉB�@B�
B��B��B�xB�GB�
By�B^�BFVB8BcB'BB�B
�}B
�B
��B
�HB
�B
��B
�+B
�B
�B
�B
��B
w�B
`B
N�B
9B
%�B
�B	��B	�NB	�BB	�6B	�0B	�$B	�B	��B	��B	ΤB	�hB	�B	��B	��B	��B	�dB	�KB	�B	��B	��B	�B	s�B	l^B	c(B	G�B	=HB	4B	*�B	$�B	�B	+B	�B��B��B�B��B�]B�-B��BɜB�eB�MB�B��B��B��B��B��B��B��B�zB�[B�IB�Bv�BfQB^ B`-Ba3B`-B^ B]B[BZBW�BW�BZB`-B]BZ	BW�BU�BS�BR�BQ�BP�BN�BM�BI�BI�BG�BG�BF�BE�BC�BAxB?lB<ZB7<B50B3$B,�B0B+�B(�B&�B&�B$�B%�B#�B"�B%�B"�B!�B!�B!�B �B#�B!�B!�B!�B#�B!�B!�B!�B!�B$�B#�B&�B#�B$�B&�B%�B$�B$�B"�B"�B$�B)�B(�B-B0B0B0B1B4.B55B7AB8GB8GB8GB<`B=fB=fB@xB?rB@xB@xBA~BA~BC�BC�BH�BG�BG�BG�BH�BJ�BJ�BJ�BJ�BK�BK�BP�BP�BT�BT�BT�BT�BZB\B]#B`6B`6Ba<BdNBf[BhgBm�Bn�Bn�Br�Bt�B{�B�B�B�B�B�B�B�B�B�$B�1B�6B�BB�BB�BB�[B�aB�aB�mB�B��B��B��B��B��B��B��B��B��B�B�B�BB�[B�aB�yBǝBȣBɪB̼B��B��B��B�B�B�6B�xB��B��B��B��B��B��B	 �B	B		#B	�B	B	;B	TB	fB	rB	xB	�B	�B	�B	�B	!�B	$�B	'�B	+�B	+�B	,�B	-�B	/B	-�B	,�B	*�B	'�B	)�B	/B	3B	9AB	=ZB	9AB	=ZB	BxB	H�B	K�B	L�B	M�B	N�B	P�B	P�B	P�B	P�B	Q�B	U�B	V�B	ZB	ZB	ZB	ZB	\B	]B	_#B	[
B	ZB	a/B	c;B	dAB	eGB	gTB	mxB	q�B	p�B	o�B	o�B	q�B	r�B	s�B	t�B	x�B	y�B	z�B	}�B	~�B	��B	�B	�B	�B	�B	�5B	�FB	�YB	�_B	�YB	�SB	�NB	�YB	�eB	�kB	�kB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�4B	�:B	�@B	�SB	�_B	�eB	�kB	�rB	�rB	�rB	�xB	�xB	�~B	ńB	ǐB	ʢB	ȖB	ʢB	̮B	ʹG�O�B	��B	��B	�/B
�B

QB
�B
�B
#LB
*B
1�B
9�B
A2B
C
B
H�B
O�B
U�B
YB
^�B
f�B
iTB
lL11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.13 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144142022020411441420220204114414  AO  ARCAADJP                                                                    20200618141345    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141345  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141345  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114414  IP                  G�O�G�O�G�O�                