CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:44Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               KA   AO  20111205113645  20190522121837  1901_5055_075                   2C  D   APEX                            2140                            040306                          846 @��|B��1   @��}��@@,�G�z��c��vȴ91   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�ff@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DSfDS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dv�fDy�3D�fD�C3D�p D��3D���D�6fD�S3D��3D���D��D�� D��3D���D�,�D�y�D�fD��fD� D�` D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @`  @�33@�ffAffA6ffAVffAvffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B%��B-��B5��B=��BE��BM��BU��B]��Be��Bm��Bu33B}��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�33B�ffB���B���B���CffCffCffCffC	ffCffCffCffCffCffCffCffCffCffCffCffC!ffC#ffC%ffC'ffC)ffC+ffC-ffC/ffC1ffC3ffC5ffC7ffC9ffC;ffC=ffC?ffCAffCCffCEffCGffCIffCKffCMffCOffCQffCSffCUffCWffCYffC[ffC]ffC_ffCaffCcffCeffCgffCiffCkffCmffCoffCqffCsffCuffCwffCyffC{ffC}ffCffC��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�� C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C³3Có3Cĳ3Cų3CƳ3Cǳ3Cȳ3Cɳ3Cʳ3C˳3C̳3Cͳ3Cγ3Cϳ3Cг3Cѳ3Cҳ3Cӳ3CԳ3Cճ3Cֳ3C׳3Cس3Cٳ3Cڳ3C۳3Cܳ3Cݳ3C޳3C߳3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C��3C�3C�fC�3C�3C�3C�3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D Y�D ٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚD	Y�D	ٚD
Y�D
ٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDS3DٚDY�DٚDY�DٚDY�DٚDY�DٚD Y�D ٚD!Y�D!ٚD"Y�D"ٚD#Y�D#ٚD$Y�D$ٚD%Y�D%ٚD&Y�D&ٚD'Y�D'ٚD(Y�D(ٚD)Y�D)ٚD*Y�D*ٚD+Y�D+ٚD,Y�D,ٚD-Y�D-ٚD.Y�D.ٚD/Y�D/ٚD0Y�D0ٚD1Y�D1ٚD2Y�D2ٚD3Y�D3ٚD4Y�D4ٚD5Y�D5ٚD6Y�D6ٚD7Y�D7ٚD8Y�D8ٚD9Y�D9ٚD:Y�D:ٚD;Y�D;ٚD<Y�D<ٚD=Y�D=ٚD>Y�D>ٚD?Y�D?ٚD@Y�D@ٚDAY�DAٚDBY�DBٚDCY�DCٚDDY�DDٚDEY�DEٚDFY�DFٚDGY�DGٚDHY�DHٚDIY�DI�3DJY�DJٚDKY�DKٚDLY�DLٚDMY�DMٚDNY�DNٚDOY�DOٚDPY�DPٚDQY�DQٚDRY�DR� DSY�DSٚDTY�DTٚDUY�DUٚDVY�DVٚDWY�DWٚDXY�DXٚDYY�DYٚDZY�DZٚD[Y�D[ٚD\Y�D\ٚD]Y�D]ٚD^Y�D^ٚD_Y�D_ٚD`Y�D`ٚDaY�DaٚDbY�DbٚDcY�DcٚDdY�DdٚDeY�DeٚDfY�DfٚDgY�DgٚDhY�DhٚDiY�DiٚDjY�DjٚDkY�DkٚDlY�DlٚDmY�DmٚDnY�DnٚDoY�DoٚDpY�DpٚDqY�DqٚDrY�DrٚDsY�DsٚDtY�DtٚDuY�DuٚDvY�Dv� Dy��D��3D�0 D�\�D�� D��fD�#3D�@ D�� D��fD�	�D�l�Dǰ D�ٚD��D�ffD��3D��3D���D�L�D�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�v�A�n�A�n�A�l�A�hsA�XA�ZA�C�A�5?A�1'A�/A�-A�+A�-A� �A�bA�VA�
=A�
=A�%A�A�A�A���A���A���A��A��HA��
A�&�A�{A�ĜA�n�A�A�A�1Aɴ9AɍPA�v�A�VA�C�A���A���AȸRAȧ�A�jA�G�A�/A���AǏ\A��HAƇ+A�;dAƗ�AŶFA�oA�r�A��HA�&�A�bA��DA��#A�%A���A��7A��A�K�A���A�|�A���A�VA�^5A���A���A�7LA��A��+A�K�A�5?A�v�A�z�A��-A�`BA�{A�ffA�G�A��DA��+A���A���A��A���A��A�
=A���A�Q�A�C�A�/AdZAy�-At�RAr  Ak?}Aa�#A\�jAXbAShsAM�AGS�AC`BAB{A@�+A=�#A<��A;O�A:1A8�A8=qA7�;A7��A8^5A9�A9\)A8��A7C�A5�A37LA1l�A/�FA-��A*^5A)A)"�A(�uA(�A(��A)/A)�A(�9A(��A(��A(ZA'�-A&jA#C�A"VA!��A �A ZAƨAM�A�AdZA?}AG�AVAG�A��A(�A�+A{A�FA��A$�A1'AQ�A  A�+AA��A�RA�jAȴAĜA�9AbNA  A�^A��A�A~�A�-Al�A`BA\)A/AoA�`A=qA�AA��A|�A&�A��A�A�mA1A��AA�A&�A
M�A	/A	&�A	"�A	/A	33A�A\)A�9A�A�^AO�A�jAv�A�wA"�AO�A�`A~�A1A
=A �RA �9A v�A b@�t�@�5?@��@�j@��@�S�@��H@��\@�-@��^@�/@���@��/@�hs@��`@�t�@�~�@�hs@�hs@�V@�I�@��@�@�@�J@�hs@��D@�w@�o@��@��T@�`B@���@�bN@�(�@�P@�ȴ@�-@�p�@�G�@�%@�I�@��m@�t�@�o@�ȴ@�~�@�@�^@䛦@�@�j@�1@�F@�33@���@�~�@�-@��@�&�@�Q�@�;d@��@ܛ�@�j@�b@۶F@�
=@�-@�$�@�-@ّh@�z�@׾w@�K�@�~�@��@�@��@�I�@ӍP@�o@��@�-@�7L@д9@��m@�+@Ο�@�{@͉7@��/@�bN@��@˅@�+@��y@ʇ+@�J@ɺ^@ɡ�@��@ȃ@�1@�K�@���@Ɨ�@�ff@�5?@Ų-@őh@�x�@�O�@��`@�  @öF@�t�@°!@�5?@��T@��^@��@��u@��@��;@�
=@�ȴ@���@�E�@�@��@���@�A�@���@��w@���@�C�@��\@��@��#@�x�@��/@��m@�|�@�+@���@�V@�M�@��T@�G�@��9@�9X@��
@�t�@��@�ȴ@�v�@��@��7@��/@�z�@��;@�dZ@�@��R@�V@�@��#@�x�@��@���@���@���@��@�Q�@��@��;@���@�o@���@�=q@��^@��@���@��`@�Ĝ@��D@�Q�@��@��@�33@�
=@���@���@�ff@�^5@�E�@��#@�O�@�V@���@��@�b@�S�@�ȴ@�$�@�@��#@�p�@��@���@�(�@��m@���@��@�dZ@��@��\@�M�@�5?@�{@��#@��@��@��@�Q�@�9X@�1'@�b@�ƨ@���@��P@�dZ@�33@�
=@���@�=q@�X@��D@�Z@�Q�@�I�@� �@��m@��w@���@�l�@�;d@�@�n�@��#@�`B@�%@���@��u@�1@�t�@�K�@�K�@�C�@�@��+@�=q@��T@�hs@���@�1'@��R@��`@�z�@~��@x1'@o+@e�@\��@UV@MV@E/@=�@6E�@.E�@'�P@#@�h@K�@(�@�P@S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�v�A�n�A�n�A�l�A�hsA�XA�ZA�C�A�5?A�1'A�/A�-A�+A�-A� �A�bA�VA�
=A�
=A�%A�A�A�A���A���A���A��A��HA��
A�&�A�{A�ĜA�n�A�A�A�1Aɴ9AɍPA�v�A�VA�C�A���A���AȸRAȧ�A�jA�G�A�/A���AǏ\A��HAƇ+A�;dAƗ�AŶFA�oA�r�A��HA�&�A�bA��DA��#A�%A���A��7A��A�K�A���A�|�A���A�VA�^5A���A���A�7LA��A��+A�K�A�5?A�v�A�z�A��-A�`BA�{A�ffA�G�A��DA��+A���A���A��A���A��A�
=A���A�Q�A�C�A�/AdZAy�-At�RAr  Ak?}Aa�#A\�jAXbAShsAM�AGS�AC`BAB{A@�+A=�#A<��A;O�A:1A8�A8=qA7�;A7��A8^5A9�A9\)A8��A7C�A5�A37LA1l�A/�FA-��A*^5A)A)"�A(�uA(�A(��A)/A)�A(�9A(��A(��A(ZA'�-A&jA#C�A"VA!��A �A ZAƨAM�A�AdZA?}AG�AVAG�A��A(�A�+A{A�FA��A$�A1'AQ�A  A�+AA��A�RA�jAȴAĜA�9AbNA  A�^A��A�A~�A�-Al�A`BA\)A/AoA�`A=qA�AA��A|�A&�A��A�A�mA1A��AA�A&�A
M�A	/A	&�A	"�A	/A	33A�A\)A�9A�A�^AO�A�jAv�A�wA"�AO�A�`A~�A1A
=A �RA �9A v�A b@�t�@�5?@��@�j@��@�S�@��H@��\@�-@��^@�/@���@��/@�hs@��`@�t�@�~�@�hs@�hs@�V@�I�@��@�@�@�J@�hs@��D@�w@�o@��@��T@�`B@���@�bN@�(�@�P@�ȴ@�-@�p�@�G�@�%@�I�@��m@�t�@�o@�ȴ@�~�@�@�^@䛦@�@�j@�1@�F@�33@���@�~�@�-@��@�&�@�Q�@�;d@��@ܛ�@�j@�b@۶F@�
=@�-@�$�@�-@ّh@�z�@׾w@�K�@�~�@��@�@��@�I�@ӍP@�o@��@�-@�7L@д9@��m@�+@Ο�@�{@͉7@��/@�bN@��@˅@�+@��y@ʇ+@�J@ɺ^@ɡ�@��@ȃ@�1@�K�@���@Ɨ�@�ff@�5?@Ų-@őh@�x�@�O�@��`@�  @öF@�t�@°!@�5?@��T@��^@��@��u@��@��;@�
=@�ȴ@���@�E�@�@��@���@�A�@���@��w@���@�C�@��\@��@��#@�x�@��/@��m@�|�@�+@���@�V@�M�@��T@�G�@��9@�9X@��
@�t�@��@�ȴ@�v�@��@��7@��/@�z�@��;@�dZ@�@��R@�V@�@��#@�x�@��@���@���@���@��@�Q�@��@��;@���@�o@���@�=q@��^@��@���@��`@�Ĝ@��D@�Q�@��@��@�33@�
=@���@���@�ff@�^5@�E�@��#@�O�@�V@���@��@�b@�S�@�ȴ@�$�@�@��#@�p�@��@���@�(�@��m@���@��@�dZ@��@��\@�M�@�5?@�{@��#@��@��@��@�Q�@�9X@�1'@�b@�ƨ@���@��P@�dZ@�33@�
=@���@�=q@�X@��D@�Z@�Q�@�I�@� �@��m@��w@���@�l�@�;d@�@�n�@��#@�`B@�%@���@��u@�1@�t�@�K�@�K�@�C�@�@��+@�=q@��T@�hs@���@�1'@��R@��`@�z�@~��@x1'@o+@e�@\��@UV@MV@E/@=�@6E�@.E�@'�P@#@�h@K�@(�@�P@S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
Q�B
Q�B
Q�B
R�B
R�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
Q�B
R�B
Q�B
Q�B
Q�B
P�B
P�B
S�B
VB
XB
W
B
VB
S�B
T�B
S�B
Q�B
O�B
I�B
G�B
H�B
L�B
G�B
C�B
C�B
B�B
A�B
@�B
I�B
�=B
�B
r�B
dZB
T�B
\)B
q�B
~�B
��B
�5B
��B{B�B�B{B1BB
��B
�B
�B
�mB
�/B
�BB
�B8RBP�BP�BS�B��B��B�=Bw�Bl�B[#B@�B�BB
�B
�BB
�;B
��B
�LB
�1B
H�B
�B	�B	��B	�B	�JB	|�B	XB	2-B	�B	JB	B�mB��BB�jB�XB�'B�!B��B��B��B�B�yB��B	�B	M�B	l�B	n�B	iyB	e`B	[#B	VB	P�B	K�B	@�B	I�B	R�B	_;B	o�B	��B	�B	�!B	�B	�dB	ĜB	��B	��B	��B	ǮB	��B	ɺB	ŢB	��B	��B	��B	�B	�TB	�yB	�B	��B	�B	�NB	��B	ĜB	��B	�}B	��B	�B	�B	�/B	�5B	��B	��B	ɺB	�B	�/B	�NB	�HB	�HB	�;B	�5B	�/B	�/B	�B	�sB	�ZB	�sB	�yB	�B	��B	��B	��B	�B	�B	��B
  B
%B
+B
	7B
%B
JB
\B
hB
bB
\B
JB
%B	��B	��B
B
B
	7B
B	��B	��B	�B	��B	�B	�B	�B	�B	�sB	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	��B
B
B
B
B	��B
B
  B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
+B
1B
1B
+B
+B
+B
1B
+B
1B
1B
1B
1B
1B
1B
	7B

=B
	7B

=B

=B

=B
DB
JB
DB
DB
DB
JB
JB
DB
DB
DB
JB
JB
PB
\B
bB
bB
bB
\B
\B
bB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
uB
uB
{B
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
+B
5?B
:^B
@�B
D�B
H�B
K�B
P�B
T�B
\)B
`BB
e`B
iyB
m�B
q�B
w�B
{�B
~�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
S�B
R�B
R�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
Q�B
R�B
Q�B
Q�B
R�B
S�B
VB
VB
XB
YB
XB
XB
T�B
VB
T�B
R�B
Q�B
J�B
H�B
I�B
M�B
H�B
D�B
D�B
D�B
C�B
A�B
H�B
�JB
�+B
t�B
ffB
W
B
_;B
~�B
�B
��B
�mB1B�B�B�B�BVB1BB
��B
�B
�B
�BB
�HB
�B9XBVBYBS�B�B�-B�hB{�Bs�BdZBK�B#�BVB
��B
�ZB
�ZB
�B
ÖB
��B
R�B
"�B	��B	�B	�XB	�uB	�PB	m�B	?}B	'�B	�B	JB�B�
BŢB��B��B�?B�9BŢB��B��B�B�B��B	�B	N�B	n�B	r�B	l�B	k�B	_;B	ZB	VB	S�B	B�B	K�B	T�B	_;B	n�B	��B	�B	�'B	�!B	�dB	ŢB	��B	�B	�B	��B	��B	��B	ǮB	��B	��B	��B	�)B	�ZB	�yB	�B	��B	�B	�mB	�B	ƨB	B	�}B	��B	�B	�B	�5B	�NB	�
B	��B	��B	�B	�/B	�NB	�NB	�NB	�BB	�;B	�;B	�/B	�B	�B	�`B	�sB	�yB	�B	��B	��B	��B	�B	�B	��B
B
+B
	7B
JB
%B
JB
bB
oB
hB
hB
\B

=B	��B	��B
B
B
JB
1B	��B	��B	�B	��B	��B	�B	�B	�B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
B
%B
B
B	��B
B
B	��B
B
B
  B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B
  B	��B
B
  B	��B	��B	��B
  B
B	��B	��B
  B
B
  B
  B
  B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	��B	�B	�B	��B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
	7B
	7B
+B
1B
1B
	7B
1B

=B
	7B
1B
	7B
	7B
	7B

=B
DB

=B
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
JB
JB
JB
PB
PB
VB
bB
hB
hB
hB
bB
bB
bB
oB
oB
oB
oB
oB
hB
oB
oB
oB
oB
uB
uB
oB
oB
{B
{B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
$�B
+B
5?B
:^B
@�B
E�B
H�B
K�B
P�B
VB
\)B
aHB
ffB
iyB
n�B
r�B
x�B
{�B
~�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<#�
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
<D��<#�
<#�
<#�
<#�
<49X<#�
<49X<#�
<#�
<#�
<#�
<49X<�o<#�
<#�
<D��<e`B<D��<#�
<u<�1<D��<49X<#�
<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.6 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250332012011312503420120113125034  AO  ARGQ                                                                        20111205113645  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113645  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125034  IP                  G�O�G�O�G�O�                