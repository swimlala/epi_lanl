CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:14:05Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141405  20220204114421  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               iA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��^H?�:1   @��^��`�@6�M����c4     1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    iA   B   B   @�  @�  @���A   A@  A`  A�  A�33A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�33B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D[��D\� D]  D]y�D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy��D�!�D�H�D�\D��fD�%qD�PRD��=D��qD�HD�W\D���DǗ�D��D�UDڥD���D��D�[3D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��
@��
@���A�A=�A]�A}�A�(�A�(�A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'�GB/�GB7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB�WB��B��qB��qB��qB��B��qB��>B��qB��qB��qB��qB��qB��qB��qBÊ>BǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C�RC޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)�D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6�D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[�HD\w�D\��D]qHD]��D^w�D^��D_w�D_��D`w�D`��Da~Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt�HDy�HD��D�D�D�{3D��=D�!HD�L)D��D��HD�D�S3D���DǓ�D��D�P�Dڠ�D��D��D�W
D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�%A���A���A�A�%A�1A�
=A�
=A�JA�oA�{A�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�oA�  A���A��Aٗ�A��
A�v�A��Aʏ\A���AǾwA���A��mA�A�"�A�hsA�1A�jA��/A�;dA�bA��`A��A�G�A��A�O�A��RA��A�^5A�A��wA���A�^5A��`A�E�A�9XA�A�{A�G�A��A���A��A�p�A��A���A�n�A��-A��#A�t�A� �A��FA��RA�&�A��#A���A�$�A���A�{A�/A���A�dZA���A��A�;dA��A��A��DA��A�jA�ZA�K�A��/A�t�A���A��^A��A�+A���A�-A��`A�=qA�G�A���A��jA�t�A���A�1'A��mA�`BA��jA��A�
=A�Q�A�bNA��!A�XA���A��mAl�A}�
A|�/A{�FAx��Au"�Ar��Aqx�Ao�wAmK�AjjAhz�Ae�AbJA_x�A]"�AZ��AX�AW%AT��AR$�AO�TANz�AMƨAMAK�
AI�hAHAF�uADA�AC��AB��AA�A@�A=ƨA<  A;��A;C�A:n�A9��A8��A8E�A7�FA6=qA5&�A4jA3C�A1+A.�uA-
=A+��A+��A+dZA*��A(�/A'�A&1'A%/A$��A$$�A#�A!�#A ��A Q�A��At�A�A�\A9XA"�A�/Av�A�A|�A��A�
A�^A��AK�AȴAZA�-A�HAv�A��A�A�AjA\)A�`AE�A\)A
�DA	�FA��A�jAbNA��Az�A�;A{A�`A�9A�\A�
A ��@�ƨ@��@�9X@�C�@�hs@�j@���@���@�`B@�@�o@�@�x�@��@�%@���@�9@�1@@�j@�!@�V@�J@��@�
=@旍@�-@���@��@�%@���@�ƨ@߮@�Z@�V@�K�@��@�w@�P@�G�@���@��/@߅@�`B@��`@�b@�@ו�@���@�Q�@���@�?}@�bN@�Z@���@�
=@�p�@���@�\)@�|�@�$�@�"�@�J@�p�@�&�@�Z@�9X@�Z@�9X@�Ĝ@��D@��@��y@��\@�n�@�+@��@���@�9X@��;@���@�t�@�dZ@�Ĝ@��@�33@�S�@�\)@�t�@�b@��@���@���@��-@���@��@��
@�t�@���@�S�@�;d@�Q�@���@�C�@���@�/@�G�@�5?@���@�V@��@�9X@���@��@�K�@�J@�n�@�E�@��u@�(�@�V@���@��;@�ȴ@���@��m@��h@��;@�n�@�@���@�`B@��F@��@�Ĝ@�\)@��@�V@��@�C�@�"�@�C�@�|�@�|�@�l�@�S�@�o@��\@�E�@��@���@��-@��@�&�@��9@���@�r�@��D@���@�bN@�Z@�I�@�(�@��m@���@�l�@��@���@�^5@�-@�-@�J@��@���@��@�-@�$�@��@��^@��@�hs@�X@�7L@���@���@�z�@�Q�@�(�@��m@��@���@���@�\)@��@��y@��\@�=q@�$�@�J@��@���@��7@�hs@�?}@�/@�V@�%@�Ĝ@���@�r�@�I�@�1'@�(�@�1@��;@��;@��;@��
@��@�|�@�\)@�C�@�33@��@��R@�^5@�5?@�5?@�=q@�5?@�5?@���@���@��h@���@�X@�?}@�&�@�V@��@�z�@�I�@���@���@�dZ@�S�@�33@�+@��@���@�^5@��@��#@���@��@�G�@�&�@�v�@yVm@s�@lѷ@d�v@\�@Yhs@Q|@L"h@E��@>5?@7�@2_@*W�@&a|@ r�@�Y@�r@;�@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�%A���A���A�A�%A�1A�
=A�
=A�JA�oA�{A�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�oA�  A���A��Aٗ�A��
A�v�A��Aʏ\A���AǾwA���A��mA�A�"�A�hsA�1A�jA��/A�;dA�bA��`A��A�G�A��A�O�A��RA��A�^5A�A��wA���A�^5A��`A�E�A�9XA�A�{A�G�A��A���A��A�p�A��A���A�n�A��-A��#A�t�A� �A��FA��RA�&�A��#A���A�$�A���A�{A�/A���A�dZA���A��A�;dA��A��A��DA��A�jA�ZA�K�A��/A�t�A���A��^A��A�+A���A�-A��`A�=qA�G�A���A��jA�t�A���A�1'A��mA�`BA��jA��A�
=A�Q�A�bNA��!A�XA���A��mAl�A}�
A|�/A{�FAx��Au"�Ar��Aqx�Ao�wAmK�AjjAhz�Ae�AbJA_x�A]"�AZ��AX�AW%AT��AR$�AO�TANz�AMƨAMAK�
AI�hAHAF�uADA�AC��AB��AA�A@�A=ƨA<  A;��A;C�A:n�A9��A8��A8E�A7�FA6=qA5&�A4jA3C�A1+A.�uA-
=A+��A+��A+dZA*��A(�/A'�A&1'A%/A$��A$$�A#�A!�#A ��A Q�A��At�A�A�\A9XA"�A�/Av�A�A|�A��A�
A�^A��AK�AȴAZA�-A�HAv�A��A�A�AjA\)A�`AE�A\)A
�DA	�FA��A�jAbNA��Az�A�;A{A�`A�9A�\A�
A ��@�ƨ@��@�9X@�C�@�hs@�j@���@���@�`B@�@�o@�@�x�@��@�%@���@�9@�1@@�j@�!@�V@�J@��@�
=@旍@�-@���@��@�%@���@�ƨ@߮@�Z@�V@�K�@��@�w@�P@�G�@���@��/@߅@�`B@��`@�b@�@ו�@���@�Q�@���@�?}@�bN@�Z@���@�
=@�p�@���@�\)@�|�@�$�@�"�@�J@�p�@�&�@�Z@�9X@�Z@�9X@�Ĝ@��D@��@��y@��\@�n�@�+@��@���@�9X@��;@���@�t�@�dZ@�Ĝ@��@�33@�S�@�\)@�t�@�b@��@���@���@��-@���@��@��
@�t�@���@�S�@�;d@�Q�@���@�C�@���@�/@�G�@�5?@���@�V@��@�9X@���@��@�K�@�J@�n�@�E�@��u@�(�@�V@���@��;@�ȴ@���@��m@��h@��;@�n�@�@���@�`B@��F@��@�Ĝ@�\)@��@�V@��@�C�@�"�@�C�@�|�@�|�@�l�@�S�@�o@��\@�E�@��@���@��-@��@�&�@��9@���@�r�@��D@���@�bN@�Z@�I�@�(�@��m@���@�l�@��@���@�^5@�-@�-@�J@��@���@��@�-@�$�@��@��^@��@�hs@�X@�7L@���@���@�z�@�Q�@�(�@��m@��@���@���@�\)@��@��y@��\@�=q@�$�@�J@��@���@��7@�hs@�?}@�/@�V@�%@�Ĝ@���@�r�@�I�@�1'@�(�@�1@��;@��;@��;@��
@��@�|�@�\)@�C�@�33@��@��R@�^5@�5?@�5?@�=q@�5?@�5?@���@���@��h@���@�X@�?}@�&�@�V@��@�z�@�I�@���@���@�dZ@�S�@�33@�+@��@���@�^5@��@��#@���@��@�G�G�O�@�v�@yVm@s�@lѷ@d�v@\�@Yhs@Q|@L"h@E��@>5?@7�@2_@*W�@&a|@ r�@�Y@�r@;�@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
"�B
#�B
#�B
#�B
#�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
"�B
"�B
!�B
�B
hB
�B
#�B
0!B
@�B
I�B
]/B
jB
k�B
p�B
s�B
�%B
��B
��B
�!B
ǮB
�TB
��B
��B  B�B1'BE�BO�BW
BZBcTBp�B� B��B��B�'B��B��B�TB�B��BBB	7BoB�B�B�B#�B(�B%�B"�B%�B%�B$�B"�B$�B%�B �B �B�B�B�B �B#�B�B�B\B+BB  B��B��B+BbB��B�B�B��B�}B�!B��Bw�B_;B=qB#�B
=B
��B
�B
B
�?B
��B
��B
�B
hsB
L�B
=qB
0!B
'�B
 �B
DB	�B	�;B	��B	ǮB	�FB	��B	�hB	}�B	ffB	VB	F�B	49B	(�B	�B	PB��B�B�ZB�HB�5B�
B��BB�jB�3B�B��B��B��B��B�{B�uB�{B�{B��B�hB�hB�oB�VB�hB�uB�oB�hB�\B�PB�7B�=B�JB�JB�7B�%B�B�B�B�B� B}�B~�B}�B}�B}�B|�B|�B{�Bz�B~�Bz�Bz�B{�Bz�By�Bx�By�By�By�Bx�By�By�Bw�Bw�Bw�Bx�Bw�Bv�Bu�Bx�Bz�B|�By�Bu�Bs�Br�Br�Bt�Bs�BiyBdZBe`Be`BjBk�Bq�Bq�Br�Bq�Bu�By�B{�B{�B{�B}�B�B�B�7B�7B�7B�DB�DB�DB�=B�PB�PB�JB�JB�VB�\B�PB�PB�JB�JB�JB�JB�\B�oB��B��B�LB�jB�wBBÖB��B��B��B��B�
B�B�B�B��B��BƨB��B��B�B�?B�?B�B�B�B�!B�^B�9B�LB�XB�^B�jB�}BÖBŢB��B��B��B��B��B��B�)B�yB��B��B��B�B��B��B�B�B�B�B�B�B��B	B	B	B	B	B	B	B	1B	
=B	JB	hB	PB	DB	
=B	bB	 �B	%�B	0!B	7LB	5?B	33B	6FB	1'B	;dB	D�B	C�B	G�B	H�B	G�B	E�B	=qB	-B	(�B	&�B	!�B	49B	1'B	(�B	$�B	%�B	9XB	<jB	7LB	J�B	T�B	K�B	:^B	�B	�B	�B	�B	�B	"�B	%�B	(�B	,B	/B	0!B	33B	6FB	9XB	:^B	<jB	?}B	D�B	H�B	J�B	M�B	P�B	R�B	T�B	VB	XB	]/B	_;B	`BB	aHB	aHB	bNB	dZB	e`B	ffB	jB	m�B	p�B	v�B	|�B	� B	�B	�B	�B	�B	�+B	�7B	�DB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�?B	�FB	�FB	�RB	�RB	�XB	�dB	�jB	�jB	�jB	�qB	�qB	�wB	�wB	�}B	��B	��B	��B	�}B	��B	��B	��B	��B	B	ŢB	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�)B	�)B	�B	�B
B
�B
=B
(�B
0�B
6�B
8�B
=�B
E9B
JXB
N�B
T�B
ZkB
]�B
a-B
e�B
i�B
n�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&9B
6�B
?�B
SDB
`�B
a�B
f�B
i�B
|7B
��B
��B
�1B
��B
�`B
��B
��B
�
B�B'.B;�BE�BMBP!BYXBf�BvB��B��B�%B��B��B�OB�B��B�B�B�0BgB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BWB�'B�B��B��B��B�(B_B��B�B�B��B��B�&B��Bm�BUGB3�B�B QB
��B
�B
��B
�ZB
��B
��B
y2B
^�B
B�B
3�B
&HB
B
�B
oB	��B	�jB	�"B	��B	�xB	��B	��B	t+B	\�B	L?B	<�B	*wB	5B	�B	�B�+B��BڠB׎B�{B�QB�B��B��B�~B�_B�HB�)B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B�B|wB{qBx_Bx_BwYBvSBtHBuNBtHBtHBtHBsBBsBBr;Bq5BuNBq6Bq6Br<Bq6Bp1Bo+Bp1Bp1Bp1Bo+Bp1Bp1Bn%Bn%Bn%Bo+Bn&Bm BlBo,Bq8BsEBp2BlBjBiBiBkBjB_�BZ�B[�B[�B`�Ba�BhBhBi
BhBlBp4Br@Br@Br@BtMBykB{xB�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�TB��B��B��B��B��B�B�(B�:B�AB�_B�qB�rB�eB�;B�#B��B�VB�PB�gB��B��B�tB�bB�nB�{B��B��B��B��B��B��B��B��B��B�%B�7B�DB�DB�=B�JBҀB��B�B�B�B�B�B�B�B��B��B��B��B� B�*B�`B�mB�aB�mB�sB�aB�gB��B	 �B	�B	�B	�B	�B	 �B	�B	B	5B	&rB	-�B	+�B	)�B	,�B	'yB	1�B	:�B	9�B	=�B	?B	=�B	;�B	3�B	#aB	IB	<B	B	*�B	'zB	JB	1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�B	�B	�B	B	&B	7B	JB	"\B	%oB	&uB	)�B	,�B	/�B	0�B	2�B	5�B	:�B	?B	AB	D%B	G7B	ICB	KOB	LUB	NaB	S�B	U�B	V�B	W�B	W�B	X�B	Z�B	[�B	\�B	`�B	c�B	f�B	mB	s<B	vNB	wTB	zgB	{mB	{mB	}yB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�5B	�<B	�<B	�MB	�_B	�fB	�lB	�rB	�xB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�"B	�.B	�5B	�5B	�AB	�MB	�SB	�`B	�fB	�kB	�kB	�qG�O�B	�SB	��B	�eB
�B
�B
B
'B
,�B
/B
3�B
;}B
@�B
D�B
KB
P�B
S�B
WpB
\AB
`?B
e(B
j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.13 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.01(+/-0.004) in PSS-78.                                                                                                                                                                                        Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144212022020411442120220204114421  AO  ARCAADJP                                                                    20200618141405    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141405  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141405  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114421  IP                  G�O�G�O�G�O�                