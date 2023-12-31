CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:31Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112810  20190522121836  1901_5055_028                   2C  D   APEX                            2140                            040306                          846 @�n��?�1   @�n�@@,yXbM��c^� ě�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A!��A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  A�33B  B  B  B ffB(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|  C~�C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D y�D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D4��D5y�D6  D6� D7  D7� D8  D8y�D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>�fD?  D?� D@  D@� DA  DAy�DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl�fDm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  DwffDy� D�  D�<�D�ffD��3D���D�9�D�� D��fD��3D�,�D�ffD��fD�  D�33D�S3D��D���D��D�S31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @`  @���@���A  A6ffAVffAvffA�33A�33A�33A�ffA�33A�33A�33A�ffB��B��B��B  B&  B-��B5��B=��BE��BM��BU��B]��Be��Bm��Bu��B}��B���B���B���B���B�  B�  B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���CffCffCffCffC	ffCffCffCffCffCffCffCffCffCffCffCffC!ffC#ffC%ffC'ffC)ffC+ffC-ffC/ffC1ffC3ffC5ffC7ffC9ffC;ffC=ffC?ffCAffCCffCEffCGffCIffCKffCMffCO� CQffCSffCUffCWffCYffC[ffC]ffC_ffCaffCcffCeffCgffCiffCkffCmffCoffCqffCsffCuL�CwffCyffC{ffC}� CffC�� C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C³3Có3Cĳ3Cų3CƳ3Cǳ3Cȳ3Cɳ3Cʳ3C˳3C̳3Cͳ3Cγ3Cϳ3Cг3Cѳ3Cҳ3Cӳ3CԳ3Cճ3Cֳ3C׳3Cس3Cٳ3Cڳ3C۳3Cܳ3Cݳ3C޳3C߳3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C��3C�3C�� C�3C�3C�3C�3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�� C��3D S3D ٚDY�D�3DY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚD	Y�D	ٚD
Y�D
ٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDS3DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚD Y�D ٚD!Y�D!ٚD"Y�D"ٚD#Y�D#ٚD$Y�D$ٚD%Y�D%ٚD&Y�D&ٚD'Y�D'ٚD(Y�D(ٚD)Y�D)ٚD*Y�D*ٚD+Y�D+ٚD,Y�D,ٚD-Y�D-ٚD.Y�D.ٚD/Y�D/ٚD0Y�D0ٚD1Y�D1ٚD2Y�D2ٚD3Y�D3ٚD4Y�D4�3D5S3D5ٚD6Y�D6ٚD7Y�D7ٚD8S3D8ٚD9Y�D9ٚD:Y�D:ٚD;Y�D;ٚD<Y�D<ٚD=Y�D=ٚD>` D>ٚD?Y�D?ٚD@Y�D@ٚDAS3DAٚDBY�DBٚDCY�DCٚDDY�DDٚDEY�DEٚDFY�DFٚDGY�DGٚDHY�DHٚDIY�DIٚDJY�DJٚDKY�DKٚDLY�DLٚDMY�DMٚDNY�DNٚDOY�DOٚDPY�DPٚDQY�DQٚDRY�DRٚDSY�DSٚDTY�DTٚDUY�DUٚDVY�DVٚDWY�DWٚDXY�DXٚDYY�DYٚDZY�DZٚD[Y�D[ٚD\Y�D\ٚD]Y�D]ٚD^Y�D^ٚD_Y�D_ٚD`Y�D`ٚDaY�DaٚDbY�DbٚDcY�DcٚDdY�DdٚDeY�DeٚDfY�DfٚDgY�DgٚDhY�DhٚDiY�DiٚDjY�DjٚDkY�DkٚDl` DlٚDmY�DmٚDnY�DnٚDoY�DoٚDpY�DpٚDqY�DqٚDrY�DrٚDsY�DsٚDtY�DtٚDuY�DuٚDvY�DvٚDw@ Dyy�D���D�)�D�S3D�� D��fD�&fD�|�D��3D�� D��D�S3Dǳ3D���D�  D�@ Dਗ਼D��fD�	�D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�&�A�&�A�"�A�&�A�(�A�&�A�&�A�&�A�(�A�(�A�(�A�+A�+A�+A�(�A�&�A�+A�-A�-A�1'A�1'A�33A�1'A�1'A�1'A�-A�+A�+A��A�ĜA�bA���AǮA�S�A��;A�dZA�%A�p�A��TA�~�A��A��A���A�|�A²-A�=qA��FA�dZA��RA��\A�l�A�5?A���A�  A��A��A�ĜA���A���A�A��#A�|�A��PA�r�A�VA��yA��A��\A���A���A�
=A���A��A���A�33A���A�C�A���A�7LA���A��9A���A���A��A��
A�&�A��-A�G�A���A��A� �A��A��+A�I�A�"�A�n�A~��Az�Av$�At  Am��Ai�
AfI�Ae
=A`1'AZn�AXVAR^5AL  AHjAGoAF{AD��AB�A?��A<�A9t�A8�A6�HA3��A1��A.��A-+A-XA-�#A-�7A-/A-
=A,�A+A+C�A+VA)�A)?}A'�
A'C�A%�A$-A"JA�/A�AbA��A�;A&�A1'A�Av�AA`BAZA5?A��A\)A��A��A�AC�A33A7LA��A�7A��AffA�HA
=A�hA��AG�A�A�hA��A��A��A��AK�A�AJA�mA�;A�A�#At�A?}A��AȴA�+AjAA�A��A�-Al�A�A
��A
�!A
�\A
-A	��A	��A	`BA��A�+AM�A�A�AƨA��A�7A�A�+A��A%A�Az�A;dA��Ax�A {A -A r�A ��A n�A �+A V@��+@�5?@��@��j@��R@���@�33@�J@��-@�"�@���@�G�@���@�@�t�@�\)@��T@�X@�n�@�X@�@�S�@��T@��/@��@�!@�"�@���@�X@�`B@���@�j@�O�@�A�@�1@�
=@�X@���@�n�@旍@�~�@�=q@�/@�@�D@�(�@㝲@�$�@�-@��@��T@��#@�p�@�Ĝ@���@�S�@�ȴ@�M�@�hs@�&�@��`@�1'@ۅ@�;d@�"�@��@�E�@ٙ�@�/@��@أ�@�bN@��@��@�5?@ՙ�@���@�z�@��;@�t�@�K�@�K�@�^5@���@�b@�33@�ff@�$�@���@���@�t�@�v�@ɡ�@�~�@�\)@��@�{@�%@Ǯ@�J@�Q�@�@��@��/@�V@��@��@�(�@�ƨ@�S�@���@�{@�X@���@�j@���@���@��j@�j@�A�@��@�K�@�;d@�
=@���@�G�@��`@�Q�@�(�@��P@�@���@���@��\@���@���@�ff@�$�@��@���@�7L@�Ĝ@�A�@��@�dZ@��y@���@�~�@�=q@�@��#@��h@�?}@��`@���@�r�@� �@�t�@��@���@�v�@���@�@��h@�G�@��`@��D@�  @���@�dZ@�;d@��y@���@�5?@���@��h@�`B@���@�r�@�A�@�1@��F@�\)@��@���@�ff@��@�x�@��@��`@�Z@�b@��
@��P@�o@���@�=q@��@���@�Z@�  @���@�"�@�ȴ@�n�@�-@��T@��-@��^@���@��^@��@�%@��@� �@��;@��w@�|�@�@���@��7@�X@�7L@�%@���@�Q�@��;@�;d@��@��y@�n�@�=q@���@�G�@���@�A�@�b@�  @���@��@���@�-@�E�@���@���@���@��@�&�@�O�@�/@��`@�G�@���@��u@��@�I�@�I�@��@�$�@�@�5?@��\@���@��@��y@�ȴ@�{@��#@��7@�`B@�A�@���@�p�@x��@p�9@e�@\z�@R��@K�@E�T@>v�@6��@-�-@(��@"��@?}@��@dZ@\)@
�\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�&�A�&�A�"�A�&�A�(�A�&�A�&�A�&�A�(�A�(�A�(�A�+A�+A�+A�(�A�&�A�+A�-A�-A�1'A�1'A�33A�1'A�1'A�1'A�-A�+A�+A��A�ĜA�bA���AǮA�S�A��;A�dZA�%A�p�A��TA�~�A��A��A���A�|�A²-A�=qA��FA�dZA��RA��\A�l�A�5?A���A�  A��A��A�ĜA���A���A�A��#A�|�A��PA�r�A�VA��yA��A��\A���A���A�
=A���A��A���A�33A���A�C�A���A�7LA���A��9A���A���A��A��
A�&�A��-A�G�A���A��A� �A��A��+A�I�A�"�A�n�A~��Az�Av$�At  Am��Ai�
AfI�Ae
=A`1'AZn�AXVAR^5AL  AHjAGoAF{AD��AB�A?��A<�A9t�A8�A6�HA3��A1��A.��A-+A-XA-�#A-�7A-/A-
=A,�A+A+C�A+VA)�A)?}A'�
A'C�A%�A$-A"JA�/A�AbA��A�;A&�A1'A�Av�AA`BAZA5?A��A\)A��A��A�AC�A33A7LA��A�7A��AffA�HA
=A�hA��AG�A�A�hA��A��A��A��AK�A�AJA�mA�;A�A�#At�A?}A��AȴA�+AjAA�A��A�-Al�A�A
��A
�!A
�\A
-A	��A	��A	`BA��A�+AM�A�A�AƨA��A�7A�A�+A��A%A�Az�A;dA��Ax�A {A -A r�A ��A n�A �+A V@��+@�5?@��@��j@��R@���@�33@�J@��-@�"�@���@�G�@���@�@�t�@�\)@��T@�X@�n�@�X@�@�S�@��T@��/@��@�!@�"�@���@�X@�`B@���@�j@�O�@�A�@�1@�
=@�X@���@�n�@旍@�~�@�=q@�/@�@�D@�(�@㝲@�$�@�-@��@��T@��#@�p�@�Ĝ@���@�S�@�ȴ@�M�@�hs@�&�@��`@�1'@ۅ@�;d@�"�@��@�E�@ٙ�@�/@��@أ�@�bN@��@��@�5?@ՙ�@���@�z�@��;@�t�@�K�@�K�@�^5@���@�b@�33@�ff@�$�@���@���@�t�@�v�@ɡ�@�~�@�\)@��@�{@�%@Ǯ@�J@�Q�@�@��@��/@�V@��@��@�(�@�ƨ@�S�@���@�{@�X@���@�j@���@���@��j@�j@�A�@��@�K�@�;d@�
=@���@�G�@��`@�Q�@�(�@��P@�@���@���@��\@���@���@�ff@�$�@��@���@�7L@�Ĝ@�A�@��@�dZ@��y@���@�~�@�=q@�@��#@��h@�?}@��`@���@�r�@� �@�t�@��@���@�v�@���@�@��h@�G�@��`@��D@�  @���@�dZ@�;d@��y@���@�5?@���@��h@�`B@���@�r�@�A�@�1@��F@�\)@��@���@�ff@��@�x�@��@��`@�Z@�b@��
@��P@�o@���@�=q@��@���@�Z@�  @���@�"�@�ȴ@�n�@�-@��T@��-@��^@���@��^@��@�%@��@� �@��;@��w@�|�@�@���@��7@�X@�7L@�%@���@�Q�@��;@�;d@��@��y@�n�@�=q@���@�G�@���@�A�@�b@�  @���@��@���@�-@�E�@���@���@���@��@�&�@�O�@�/@��`@�G�@���@��u@��@�I�@�I�@��@�$�@�@�5?@��\@���@��@��y@�ȴ@�{@��#@��7@�`B@�A�@���@�p�@x��@p�9@e�@\z�@R��@K�@E�T@>v�@6��@-�-@(��@"��@?}@��@dZ@\)@
�\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�BB
�B
T�B
aHB
p�B
�B
�\B
��B
�3B
�LB
B
��B
�B
�`B%B)�BM�B`BBbNBm�Bx�B�B�B��B�9BŢB��B�BB�LB�-B��B�HB�
B��B�B��B��BDB%�BT�Bt�B}�Bq�B_;B[#BN�BI�B2-B33BG�BT�BF�B,B��B��B��BW
B�B
�B
�B
�qB
�B
>wB
/B
hB	�B	�B	�B	iyB	\)B	E�B	2-B	$�B	�B	B�B�mB��B��BȴB��B��B��B��B��BB�}B��B��B��B�FB�B�qB��B�yB��B��B	JB	�B	&�B	,B	1'B	=qB	?}B	>wB	=qB	5?B	'�B	{B	B	B	+B	PB	%B	B	#�B	�B	hB	\B	VB��B�B��B�B�B	B	B	  B	B	
=B	�B	1'B	B�B	N�B	\)B	dZB	x�B	� B	�+B	�PB	�bB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�-B	�9B	�RB	��B	ŢB	ǮB	��B	��B	��B	��B	�B	�B	�#B	�)B	�BB	�NB	�ZB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�HB	�;B	�#B	��B	��B	ȴB	ÖB	ǮB	��B	��B	�
B	�B	�
B	��B	��B	��B	ȴB	��B	�RB	�jB	�wB	�}B	��B	��B	��B	��B	�BB	�B	�B	�yB	�mB	�B	�B	�yB	�yB	�mB	�yB	�mB	�`B	�B	�B	�B	�B	�B	�B	�mB	�`B	�TB	�HB	�BB	�fB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
  B
  B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�sB	�yB	�B	��B	��B	��B	�B	�B	�B	�ZB	�/B	�B	�#B	�;B	�;B	�;B	�;B	�;B	�;B	�BB	�;B	�;B	�5B	�;B	�`B	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�sB	�mB	�yB	�B	�B	�yB	�sB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
B
B
%B
+B
+B
+B
+B
%B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
1B
1B
	7B
DB
DB
JB
VB
VB
\B
\B
hB
bB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
"�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
(�B
33B
8RB
>wB
D�B
I�B
O�B
S�B
XB
]/B
cTB
jB
k�B
q�B
u�B
z�B
~�B
�B
�+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�sB
 �B
W
B
cTB
r�B
�B
�hB
��B
�9B
�RB
ÖB
��B
�#B
�yB1B-BO�BdZBcTBn�By�B�%B�+B��B�?BƨB�)B�B��B��B��B�yB�#B��B�?B��BBVB%�BS�Bv�B�Bv�BcTB^5BP�BP�B5?B1'BG�BYBN�B;dB1B�5BBffB!�B
��B
�)B
ŢB
��B
H�B
9XB
!�B	�;B	�LB	�bB	o�B	l�B	N�B	;dB	'�B	"�B	DB��B�B�BB��B��B��B��B��B�
B�)B��BĜB��B�BɺB��B�-B�qB��B�B��B��B	VB	#�B	(�B	.B	6FB	@�B	E�B	A�B	E�B	;dB	1'B	 �B	+B	B	JB	hB		7B	B	+B	�B	uB	hB	�B��B�B��B��B�B	B	B	B	B		7B	 �B	0!B	A�B	M�B	\)B	cTB	x�B	�B	�+B	�PB	�bB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�3B	�?B	�XB	��B	ƨB	ȴB	��B	��B	��B	��B	�B	�#B	�)B	�/B	�HB	�TB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�NB	�HB	�;B	�B	��B	��B	ÖB	ƨB	��B	�B	�
B	�#B	�#B	��B	��B	��B	��B	ǮB	�RB	�wB	�}B	�qB	��B	��B	��B	��B	�BB	�B	�B	�B	�fB	�B	�B	�B	�B	�yB	�B	�yB	�`B	�B	�B	�B	�B	�B	�B	�yB	�fB	�TB	�ZB	�BB	�`B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�sB	�B	��B	��B	��B	��B	�B	�B	�yB	�5B	�#B	�#B	�;B	�BB	�HB	�BB	�BB	�BB	�HB	�BB	�BB	�;B	�;B	�`B	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�sB	�B	�B	�B	�B	�yB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
%B
%B
+B
1B
1B
1B
1B
+B
+B
B
B
%B
%B
+B
+B
1B
1B
1B
1B
	7B
	7B

=B
JB
JB
PB
VB
\B
bB
bB
oB
bB
oB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
 �B
�B
!�B
 �B
�B
�B
�B
"�B
'�B
(�B
)�B
)�B
+B
+B
,B
+B
+B
)�B
33B
8RB
>wB
D�B
J�B
O�B
T�B
XB
^5B
cTB
jB
l�B
q�B
v�B
z�B
~�B
�B
�+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<e`B<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<D��<49X<ě�<e`B<#�
<#�
<#�
<#�
<�o<#�
<#�
<�o<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.6 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250172012011312501720120113125017  AO  ARGQ                                                                        20111205112810  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112810  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125017  IP                  G�O�G�O�G�O�                