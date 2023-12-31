CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:25Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �`   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �h   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112423  20190522121836  1901_5055_008                   2C  D   APEX                            2140                            040306                          846 @�=L�f��1   @�=M�m��@0KI�^�c.n��O�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2fD2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>fD>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwl�Dy� D�� D�)�D�\�D�� D��fD�#3D�p D�� D���D�0 D�ffDǳ3D��fD�#3D�P D�� D�� D�,�D�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @y��@�33@�33A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBN��BVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�33B���B�33B�33B�33B�ffB�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO�3CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC�ٚC���C���C���C���C���C���C���C�ٚC���C���C���C���C���C�ٚC�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fD` D�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1l�D1��D2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=��D>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLl�DL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwS3Dy�fD��3D��D�P D��3D�ٚD�fD�c3D��3D���D�#3D�Y�DǦfD�ٚD�fD�C3D��3D��3D�  D�<�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A��HA���AָRAִ9A֩�A֋DA�t�A�ZA�C�A�7LA�&�A�oA��A���A���A�ƨA�Aպ^AնFAծAգ�AՕ�AՑhAՓuAՓuAՕ�A՗�AՓuAՍPA�x�A�G�A��AѸRAξwAŗ�A��A���A�+A�`BA�7LA�33A�E�A�A�%A�XA�|�A���A�I�A�  A���A���A�\)A�Q�A���A��;A��yA��TA�9XA��A��hA�VA�O�A�$�A�"�A�hsA���A�ZA��A�VA��-A�(�A�ƨA�v�A��A��A�dZA�5?A��PA�;dA��A�33A��A���A�ffA��A�`BA�;dA�`BA}`BAv�ArVAoVAl~�Ak+Ai�Af �A_�mA]�A]��A]��A]?}A\z�A[K�AZ5?AW%AT9XASS�AQ��AL�+AJ��AH��AGx�AG�AF9XAE��AC�ABbNA?7LA<��A:�yA9�PA6�`A4�`A3+A1�A/dZA.�RA.�jA-�TA-?}A,ZA+O�A+�7A+VA)��A&�HA#��A"�+A!�A bNA�jA�mA��A�A/AM�A&�A��Ax�A7LA�Ax�Ax�A��A�A��A�uA5?A�;A�PA+A~�AAx�A�A�RA�RAȴA�TA?}A/A
=qA�HA�wA
=A�A�7A��A��A�A�yA�A��A?}A�AVAI�At�Av�A �AdZA�AK�AC�Al�A ~�@���@���@�+@���@�hs@�-@�r�@�`B@���@��R@�O�@��@�$�@�=q@�=q@��@��@�^5@�@���@��@�$�@�7L@�-@�7@�7L@��@��@�Q�@�  @��@�
=@�V@�Ĝ@���@�@�dZ@�C�@�\)@�K�@���@�S�@�ȴ@��@���@��@���@���@� �@�o@�ȴ@�v�@އ+@�5?@�-@�@ݙ�@���@�Ĝ@ܴ9@܋D@�+@ڸR@ڗ�@�@���@ّh@ف@�7L@�r�@��@� �@�1'@�A�@���@��
@׶F@׍P@�|�@�t�@֏\@ղ-@Դ9@�  @Ԭ@�z�@��;@��@ҏ\@��@��/@���@�I�@���@�33@Ώ\@�@�Ĝ@�1'@��
@�S�@�ff@��#@Ɂ@��@� �@��;@�K�@�o@�@��y@�v�@�?}@�j@�b@���@ÍP@�+@¸R@�J@��7@�7L@��j@�A�@���@�K�@���@���@��+@�$�@��@���@�X@��/@�bN@��m@�C�@���@��\@�=q@��@���@�X@���@��u@�bN@��m@�|�@�C�@�33@�"�@�o@���@��!@�ff@��T@���@�7L@���@�j@��@��
@�S�@��@���@�E�@�{@���@���@�x�@�X@�7L@���@�Ĝ@�r�@��@��w@��P@�"�@���@�=q@��T@���@�hs@���@�j@�  @�+@���@�n�@�5?@��#@��@�?}@���@��u@�(�@��;@���@�V@���@���@�G�@���@��9@�r�@��w@�33@�o@��y@�~�@�@��^@�/@��/@���@�  @��P@�o@���@���@�V@��@���@�`B@�`B@�X@���@��9@�j@�(�@�  @��
@���@�
=@���@���@��@�X@�?}@��`@���@�Z@���@�|�@�dZ@�;d@�@��@��@���@�~�@�V@�@���@�`B@���@��9@��u@�Q�@�A�@���@��F@�t�@�o@��H@��R@�v�@�J@���@��7@�/@��@�9X@�(�@��@��;@��@�33@��H@���@���@�n�@�-@�$�@�J@��#@���@���@��@��@�X@��@�I�@�(�@� �@�33@�~�@�J@{33@o;d@dz�@^��@Yhs@P�@H  @?�w@9�@2M�@,Z@%�@;d@�H@�T@��@9X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A��HA���AָRAִ9A֩�A֋DA�t�A�ZA�C�A�7LA�&�A�oA��A���A���A�ƨA�Aպ^AնFAծAգ�AՕ�AՑhAՓuAՓuAՕ�A՗�AՓuAՍPA�x�A�G�A��AѸRAξwAŗ�A��A���A�+A�`BA�7LA�33A�E�A�A�%A�XA�|�A���A�I�A�  A���A���A�\)A�Q�A���A��;A��yA��TA�9XA��A��hA�VA�O�A�$�A�"�A�hsA���A�ZA��A�VA��-A�(�A�ƨA�v�A��A��A�dZA�5?A��PA�;dA��A�33A��A���A�ffA��A�`BA�;dA�`BA}`BAv�ArVAoVAl~�Ak+Ai�Af �A_�mA]�A]��A]��A]?}A\z�A[K�AZ5?AW%AT9XASS�AQ��AL�+AJ��AH��AGx�AG�AF9XAE��AC�ABbNA?7LA<��A:�yA9�PA6�`A4�`A3+A1�A/dZA.�RA.�jA-�TA-?}A,ZA+O�A+�7A+VA)��A&�HA#��A"�+A!�A bNA�jA�mA��A�A/AM�A&�A��Ax�A7LA�Ax�Ax�A��A�A��A�uA5?A�;A�PA+A~�AAx�A�A�RA�RAȴA�TA?}A/A
=qA�HA�wA
=A�A�7A��A��A�A�yA�A��A?}A�AVAI�At�Av�A �AdZA�AK�AC�Al�A ~�@���@���@�+@���@�hs@�-@�r�@�`B@���@��R@�O�@��@�$�@�=q@�=q@��@��@�^5@�@���@��@�$�@�7L@�-@�7@�7L@��@��@�Q�@�  @��@�
=@�V@�Ĝ@���@�@�dZ@�C�@�\)@�K�@���@�S�@�ȴ@��@���@��@���@���@� �@�o@�ȴ@�v�@އ+@�5?@�-@�@ݙ�@���@�Ĝ@ܴ9@܋D@�+@ڸR@ڗ�@�@���@ّh@ف@�7L@�r�@��@� �@�1'@�A�@���@��
@׶F@׍P@�|�@�t�@֏\@ղ-@Դ9@�  @Ԭ@�z�@��;@��@ҏ\@��@��/@���@�I�@���@�33@Ώ\@�@�Ĝ@�1'@��
@�S�@�ff@��#@Ɂ@��@� �@��;@�K�@�o@�@��y@�v�@�?}@�j@�b@���@ÍP@�+@¸R@�J@��7@�7L@��j@�A�@���@�K�@���@���@��+@�$�@��@���@�X@��/@�bN@��m@�C�@���@��\@�=q@��@���@�X@���@��u@�bN@��m@�|�@�C�@�33@�"�@�o@���@��!@�ff@��T@���@�7L@���@�j@��@��
@�S�@��@���@�E�@�{@���@���@�x�@�X@�7L@���@�Ĝ@�r�@��@��w@��P@�"�@���@�=q@��T@���@�hs@���@�j@�  @�+@���@�n�@�5?@��#@��@�?}@���@��u@�(�@��;@���@�V@���@���@�G�@���@��9@�r�@��w@�33@�o@��y@�~�@�@��^@�/@��/@���@�  @��P@�o@���@���@�V@��@���@�`B@�`B@�X@���@��9@�j@�(�@�  @��
@���@�
=@���@���@��@�X@�?}@��`@���@�Z@���@�|�@�dZ@�;d@�@��@��@���@�~�@�V@�@���@�`B@���@��9@��u@�Q�@�A�@���@��F@�t�@�o@��H@��R@�v�@�J@���@��7@�/@��@�9X@�(�@��@��;@��@�33@��H@���@���@�n�@�-@�$�@�J@��#@���@���@��@��@�X@��@�I�@�(�@� �@�33@�~�@�J@{33@o;d@dz�@^��@Yhs@P�@H  @?�w@9�@2M�@,Z@%�@;d@�H@�T@��@9X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�?B
�?B
�LB
�RB
�^B
�^B
�^B
�jB
�qB
�}B
��B
��B
��B
B
ÖB
ŢB
ĜB
ĜB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ǮB
ǮB
ǮB
ȴB
��B
��B
ɺB
ĜB
�}B
��B
��B
�LB
ŢB
��B
m�B
�7B
��B
�qB
��B9XB:^BiyB�
B
=B{B�BJB��B��B��B�B8RBN�BcTB`BBhsBu�B� B�bB�XBÖB�BB�5B�
BƨB�}B�XB�B��B��B�bBhsBL�B0!B  B��B��Be`B2-B
��B
��B
gmB
F�B
+B	��B	�JB	bNB	J�B	?}B	H�B	A�B	1'B	�B�B�fB	JB	N�B	T�B	S�B	S�B	K�B	:^B	(�B	�B	PB��B�B�B�B��B��B�B�TB��B�XB��B��B��B�oB�oB�VB�JB�uB��B�dBɺB��B�B�#B�yB�B�BƨB�^B�jB�}B�jB�3B��B��B�oB��B��B��BB��B��B��BȴB�B�;B�)B��B��B�B�)B�)B�#B�/B�HB�TB�mB�B�B��B��B�B��B��B�B�B�B��B	+B	
=B	PB	�B	�B	hB	bB	�B	"�B	'�B	#�B	 �B	"�B	$�B	"�B	%�B	/B	8RB	>wB	?}B	?}B	K�B	M�B	W
B	S�B	H�B	H�B	R�B	J�B	S�B	P�B	H�B	K�B	N�B	Q�B	S�B	VB	R�B	VB	`BB	l�B	l�B	iyB	cTB	e`B	e`B	e`B	e`B	ffB	jB	m�B	l�B	m�B	jB	hsB	iyB	k�B	o�B	q�B	q�B	q�B	u�B	u�B	w�B	z�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�1B	�7B	�=B	�=B	�=B	�7B	�=B	�DB	�DB	�JB	�PB	�VB	�VB	�VB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�3B	�3B	�?B	�FB	�RB	�RB	�RB	�RB	�dB	�wB	�}B	��B	��B	��B	B	ÖB	ÖB	ĜB	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�/B	�/B	�5B	�5B	�BB	�HB	�HB	�NB	�ZB	�ZB	�ZB	�ZB	�TB	�ZB	�ZB	�`B	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B
	7B

=B
DB

=B
DB
DB
JB
JB
PB
PB
VB
\B
\B
\B
\B
\B
\B
bB
bB
hB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
(�B
.B
5?B
9XB
A�B
F�B
M�B
R�B
ZB
]/B
aHB
gmB
l�B
p�B
t�B
y�B
~�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�?B
�FB
�RB
�XB
�^B
�^B
�dB
�qB
�wB
��B
��B
��B
B
ÖB
ĜB
ŢB
ĜB
ĜB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ǮB
ǮB
ǮB
ȴB
��B
��B
��B
ŢB
ɺB
��B
��B
�jB
��B
�B
o�B
�7B
��B
ÖB
��BA�B=qBhsB�BJB�B�BoB��B��B��B�B8RBN�BffBcTBiyBx�B�B�hB�XBB�TB�HB�)BɺB��B�jB�B��B��B��Bo�BR�B6FB+B��B�3Bk�B9XBB
�LB
m�B
W
B
�B	��B	��B	n�B	T�B	F�B	M�B	H�B	;dB	+B��B�`B	DB	Q�B	XB	W
B	W
B	S�B	A�B	,B	$�B	�B��B�B�B�B��B��B��B�mB�#B��B�B��B��B��B��B�oB�uB��B��B�qB��B��B�#B�#B�B�B�BB��B�qB�}BB��B�^B��B��B�oB��BĜB��BɺBB��B��BȴB�
B�NB�BB�B��B�B�/B�5B�5B�;B�TB�`B�sB�B�B��B��B��B��B��B�B�B�B��B	+B	JB	VB	�B	�B	{B	VB	�B	#�B	+B	&�B	#�B	#�B	'�B	#�B	%�B	/B	8RB	A�B	A�B	?}B	L�B	N�B	YB	XB	J�B	G�B	W
B	I�B	VB	S�B	I�B	K�B	N�B	R�B	VB	ZB	S�B	VB	_;B	n�B	n�B	n�B	dZB	ffB	e`B	e`B	ffB	gmB	k�B	n�B	m�B	o�B	k�B	iyB	iyB	k�B	o�B	q�B	r�B	q�B	v�B	v�B	x�B	z�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�1B	�7B	�=B	�=B	�DB	�PB	�=B	�DB	�JB	�JB	�PB	�PB	�\B	�\B	�\B	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�9B	�9B	�9B	�FB	�LB	�XB	�RB	�RB	�XB	�qB	��B	��B	��B	B	B	ÖB	ĜB	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�/B	�/B	�/B	�;B	�;B	�HB	�HB	�HB	�TB	�`B	�`B	�`B	�`B	�ZB	�`B	�`B	�fB	�mB	�mB	�mB	�mB	�yB	�yB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
	7B

=B

=B
DB
JB
DB
DB
JB
JB
PB
VB
VB
\B
bB
bB
bB
bB
bB
bB
hB
hB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
(�B
.B
6FB
9XB
A�B
F�B
M�B
S�B
[#B
]/B
aHB
gmB
l�B
p�B
t�B
y�B
~�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<u<#�
<�o<��
<�C�<�C�<D��<#�
<#�
<#�
<#�
<#�
<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250102012011312501020120113125010  AO  ARGQ                                                                        20111205112423  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112423  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125010  IP                  G�O�G�O�G�O�                