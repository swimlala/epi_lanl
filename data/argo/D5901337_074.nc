CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       j    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       r   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |$   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               JA   AO  20111205113635  20190522121836  1901_5055_074                   2C  D   APEX                            2140                            040306                          846 @��G�1   @��""$@-��+�c�r� Ĝ1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C�fC  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D   D � D!  D!� D"  D"� D"��D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DH��DIy�DI��DJ� DK  DK� DL  DL� DM  DM� DN  DNy�DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DTy�DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�fDy��D�fD�0 D�p D��3D�3D�0 D�,�D��fD�� D��D�p Dǹ�D��3D�fDڀ D��D��3D��D�VfD�i�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@`  @���@���AffA6ffAVffAvffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B%��B-��B5��B=��BE��BM��BU��B]��Be��Bm��Bu��B}��B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���CffC� CL�CffC	ffCffCffCffCffCffC� CffCffCffCffCffC!ffC#ffC%ffC'ffC)ffC+ffC-ffC/ffC1L�C3ffC5ffC7ffC9ffC;ffC=ffC?ffCAffCC� CEffCGffCIffCKffCMffCOffCQffCSffCUffCWffCYffC[ffC]ffC_ffCaffCcffCeffCgffCiffCkffCmffCoffCqffCsffCuffCwffCyffC{ffC}ffCL�C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�� C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C³3Có3Cĳ3Cų3CƳ3Cǳ3Cȳ3Cɳ3Cʳ3C˳3C̳3Cͳ3Cγ3Cϳ3Cг3CѦfCҳ3Cӳ3CԳ3Cճ3Cֳ3C׳3Cس3Cٳ3Cڳ3C۳3Cܳ3Cݳ3C޳3C߳3C�� C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C��3C�3C�3C�3C�3C�3C�3C��3C��3C��3C��3C��3C��3C��3C�� C��3C��3C��3C��3D Y�D ٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚD	Y�D	ٚD
Y�D
ٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚD` DٚDY�DٚDY�DٚDY�DٚD Y�D ٚD!Y�D!ٚD"Y�D"�3D#S3D#ٚD$Y�D$ٚD%Y�D%ٚD&Y�D&ٚD'Y�D'ٚD(Y�D(ٚD)Y�D)ٚD*` D*ٚD+Y�D+ٚD,Y�D,ٚD-Y�D-ٚD.Y�D.ٚD/Y�D/ٚD0Y�D0ٚD1Y�D1ٚD2Y�D2ٚD3Y�D3ٚD4Y�D4ٚD5Y�D5ٚD6Y�D6ٚD7Y�D7ٚD8Y�D8ٚD9Y�D9ٚD:Y�D:ٚD;Y�D;ٚD<Y�D<ٚD=Y�D=ٚD>Y�D>ٚD?Y�D?ٚD@Y�D@ٚDAY�DAٚDBY�DBٚDCY�DCٚDDY�DDٚDEY�DEٚDFY�DFٚDGY�DGٚDHY�DH�3DIS3DI�3DJY�DJٚDKY�DKٚDLY�DLٚDMY�DMٚDNS3DNٚDOY�DOٚDPY�DPٚDQY�DQٚDRY�DRٚDSY�DSٚDTS3DTٚDUY�DUٚDVY�DVٚDWY�DWٚDXY�DXٚDYY�DYٚDZY�DZٚD[Y�D[ٚD\Y�D\ٚD]Y�D]ٚD^Y�D^ٚD_Y�D_ٚD`Y�D`ٚDaY�DaٚDbY�DbٚDcY�DcٚDdY�DdٚDeY�DeٚDfY�DfٚDgY�DgٚDhY�DhٚDiY�DiٚDjS3DjٚDkY�DkٚDlY�DlٚDmY�DmٚDnY�DnٚDoY�DoٚDpY�DpٚDqY�DqٚDrY�DrٚDs` DsٚDtY�DtٚDuY�DuٚDvY�DvٚDwY�Dw� Dy�fD��3D��D�\�D�� D�  D��D��D��3D���D�fD�\�DǦfD�� D�3D�l�Dਗ਼D�� D�	�D�C3D�Vf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aϝ�A�jA͑hA�jA�Q�A�A�A�7LA�/A�$�A��A��A�oA�%A���A��A���Ḁ�A�l�A�
=AˬA�/A�
=A�A���A���A���A��A��mA��HA���AʾwAʰ!A�jA�33A���AɮAɧ�Aɡ�AɋDA�~�A�n�A�1'A�  A��mA�ĜA�bNA��HA�dZAƇ+A�~�A�ZA�O�A� �A��
A���A� �A���A��hA�A�A�ƨA���A�G�A��yA�33A��HA��A���A�ffA�A��+A��A���A�ĜA���A���A�"�A�~�A��A��/A�`BA�5?A�1'A�VA�VA�  A��A��DA�?}A��DA�ƨA}AuS�Ao�Am`BAjA�AgC�AdE�A`~�A\��AYdZAXI�AS33AOVAL�AJ��AJ��AJZAG�TAFQ�AD�AChsAB��A?K�A:��A9x�A9hsA7XA4E�A3`BA2E�A0��A-hsA,��A,1A+�A+�-A,  A,9XA,n�A,�A+`BA+G�A+%A*ffA)�A)�7A)VA(��A(VA'�#A'x�A&�`A&$�A%x�A$��A#��A"Q�A!�A ZA JA�hAjAA�A/A=qA�A��AA"�AA7LA�A��A��A�Ap�A  A�FA�A��A��A�mAA��A^5A�A��Az�AbAE�A�AS�An�AVA��A�AZA�A
��A
��A+A
ĜA
��A
�uA
��A
�DA
 �A	�
A	��A	�A	�A$�AbNAZA�+AbNA�9AZA�
Al�A5?A��A�AXA"�A��A�DA�A��A��A�FA �@��m@���A -@�ƨ@��@�=q@���@��@�(�@��P@���@���@���@��@�dZ@���@���@�hs@�x�@��7@�&�@�7L@�7L@��/@��9@���@�\)@�
=@��@�{@��@�u@�w@�|�@�33@���@��@�X@���@�@���@��H@�J@��@�V@�Q�@�@�+@�J@�h@��@�9X@�K�@�R@�-@�@���@�1'@�l�@ޏ\@��@�?}@���@�j@�1'@��@��@١�@�x�@�x�@�?}@��@ؓu@ם�@���@֗�@�^5@���@�X@�Ĝ@��@ӥ�@�\)@��@�=q@ѩ�@�/@мj@���@д9@�b@ϝ�@�C�@�@�v�@���@��@��@���@�A�@˾w@�K�@�^5@ɉ7@�&�@��`@�Q�@ǝ�@��@Ɨ�@�J@ź^@�&�@�A�@��;@�K�@\@�@���@��@�t�@�S�@�~�@��@��/@���@���@���@�V@��#@��h@�V@�Z@��
@�S�@��y@��\@�5?@�J@��-@��@��j@�z�@�j@�(�@���@�dZ@�\)@�K�@���@��T@�?}@�7L@��/@��u@��@��P@�o@�ȴ@�-@��@��@�7L@�V@��`@���@�1'@��@��F@��P@�l�@�C�@�@��@���@�=q@��@��@�/@��@���@��@�z�@�1'@���@���@�K�@��@��@���@�ff@�E�@�-@�{@��@���@�V@���@���@�Q�@��@�  @��w@�t�@�S�@�o@��\@�=q@��@���@�x�@�G�@���@��j@��D@�Q�@��w@�\)@�33@�
=@��@��R@��\@�M�@�$�@�@���@��@��`@��@�A�@�(�@���@�S�@��y@��+@�^5@�=q@�-@�J@��-@�X@�7L@��/@��u@�1'@� �@��@�l�@��R@�^5@���@���@��h@��/@�Q�@���@��@�\)@�o@��H@��!@�^5@�{@��^@�`B@�%@��/@�Q�@��@�ƨ@��@�@��h@�n�@�?}@w�@m��@b�!@XĜ@P��@I7L@@�9@:M�@5V@.E�@&�y@!%@j@  @Z@r�@/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aϝ�A�jA͑hA�jA�Q�A�A�A�7LA�/A�$�A��A��A�oA�%A���A��A���Ḁ�A�l�A�
=AˬA�/A�
=A�A���A���A���A��A��mA��HA���AʾwAʰ!A�jA�33A���AɮAɧ�Aɡ�AɋDA�~�A�n�A�1'A�  A��mA�ĜA�bNA��HA�dZAƇ+A�~�A�ZA�O�A� �A��
A���A� �A���A��hA�A�A�ƨA���A�G�A��yA�33A��HA��A���A�ffA�A��+A��A���A�ĜA���A���A�"�A�~�A��A��/A�`BA�5?A�1'A�VA�VA�  A��A��DA�?}A��DA�ƨA}AuS�Ao�Am`BAjA�AgC�AdE�A`~�A\��AYdZAXI�AS33AOVAL�AJ��AJ��AJZAG�TAFQ�AD�AChsAB��A?K�A:��A9x�A9hsA7XA4E�A3`BA2E�A0��A-hsA,��A,1A+�A+�-A,  A,9XA,n�A,�A+`BA+G�A+%A*ffA)�A)�7A)VA(��A(VA'�#A'x�A&�`A&$�A%x�A$��A#��A"Q�A!�A ZA JA�hAjAA�A/A=qA�A��AA"�AA7LA�A��A��A�Ap�A  A�FA�A��A��A�mAA��A^5A�A��Az�AbAE�A�AS�An�AVA��A�AZA�A
��A
��A+A
ĜA
��A
�uA
��A
�DA
 �A	�
A	��A	�A	�A$�AbNAZA�+AbNA�9AZA�
Al�A5?A��A�AXA"�A��A�DA�A��A��A�FA �@��m@���A -@�ƨ@��@�=q@���@��@�(�@��P@���@���@���@��@�dZ@���@���@�hs@�x�@��7@�&�@�7L@�7L@��/@��9@���@�\)@�
=@��@�{@��@�u@�w@�|�@�33@���@��@�X@���@�@���@��H@�J@��@�V@�Q�@�@�+@�J@�h@��@�9X@�K�@�R@�-@�@���@�1'@�l�@ޏ\@��@�?}@���@�j@�1'@��@��@١�@�x�@�x�@�?}@��@ؓu@ם�@���@֗�@�^5@���@�X@�Ĝ@��@ӥ�@�\)@��@�=q@ѩ�@�/@мj@���@д9@�b@ϝ�@�C�@�@�v�@���@��@��@���@�A�@˾w@�K�@�^5@ɉ7@�&�@��`@�Q�@ǝ�@��@Ɨ�@�J@ź^@�&�@�A�@��;@�K�@\@�@���@��@�t�@�S�@�~�@��@��/@���@���@���@�V@��#@��h@�V@�Z@��
@�S�@��y@��\@�5?@�J@��-@��@��j@�z�@�j@�(�@���@�dZ@�\)@�K�@���@��T@�?}@�7L@��/@��u@��@��P@�o@�ȴ@�-@��@��@�7L@�V@��`@���@�1'@��@��F@��P@�l�@�C�@�@��@���@�=q@��@��@�/@��@���@��@�z�@�1'@���@���@�K�@��@��@���@�ff@�E�@�-@�{@��@���@�V@���@���@�Q�@��@�  @��w@�t�@�S�@�o@��\@�=q@��@���@�x�@�G�@���@��j@��D@�Q�@��w@�\)@�33@�
=@��@��R@��\@�M�@�$�@�@���@��@��`@��@�A�@�(�@���@�S�@��y@��+@�^5@�=q@�-@�J@��-@�X@�7L@��/@��u@�1'@� �@��@�l�@��R@�^5@���@���@��h@��/@�Q�@���@��@�\)@�o@��H@��!@�^5@�{@��^@�`B@�%@��/@�Q�@��@�ƨ@��@�@��h@�n�@�?}@w�@m��@b�!@XĜ@P��@I7L@@�9@:M�@5V@.E�@&�y@!%@j@  @Z@r�@/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�sB
�fB
�HB
�#B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ĜB
�wB
�9B
�-B
�3B
�'B
�B
�'B
�-B
�!B
�B
��B
��B
��B
��B
�uB
�VB
�1B
��B
�XB
ƨB
��B
�RB
�-B
�?B
�3B
�?B
�}B
ŢB
ŢB
ĜB
ÖB
ɺB
��B
�
B
��B
��B
�B
�yB
�B
�B
��B
�RB
��B
��B
��B
��B
�JB
{�B
l�B
bNB
Q�B
H�B
D�B
8RB
(�B
1B	�B	��B	��B	~�B	o�B	bNB	P�B	@�B	-B	�B	%B��B�fB�
B�mB�B��B	�B	&�B	(�B	0!B	6FB	8RB	49B	�B	uB	7LB	8RB	�B	hB	�B	(�B	�B	�B	 �B	2-B	:^B	H�B	]/B	n�B	�B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�^B	�wB	�wB	�dB	ƨB	��B	�FB	�'B	�LB	�^B	�dB	�jB	ŢB	��B	��B	��B	��B	�B	�BB	�B
PB
oB
�B
�B
�B
PB
+B
+B
	7B
hB
�B
�B
�B
{B

=B
B
  B
+B
%B
hB
uB
\B

=B
DB

=B
B
%B
B
  B
  B
	7B
1B
DB
\B
oB
uB
bB
\B
\B
\B
PB
B	��B	��B
%B
%B
\B
PB
DB
DB
B
%B

=B
1B
%B
B
B
B
B
B
B	��B	��B	��B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
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
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
JB
PB
VB
\B
\B
\B
bB
bB
bB
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
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
'�B
49B
8RB
@�B
C�B
G�B
N�B
R�B
YB
^5B
aHB
ffB
jB
o�B
s�B
w�B
z�B
~�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�yB
�sB
�TB
�/B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ŢB
��B
�?B
�-B
�3B
�-B
�!B
�-B
�9B
�'B
�B
�B
��B
��B
��B
��B
�{B
�JB
��B
�qB
��B
ǮB
�qB
�?B
�FB
�dB
��B
ĜB
ǮB
ǮB
��B
��B
��B
�#B
�#B
�B
��B
�/B
��B
�NB
�NB
�B
�jB
��B
��B
��B
�-B
�hB
� B
o�B
gmB
W
B
M�B
K�B
?}B
7LB
oB	��B	�B	�-B	�+B	y�B	k�B	XB	G�B	33B	�B	1B	+B�B�5B�B�B��B	�B	,B	.B	49B	8RB	C�B	B�B	�B	{B	>wB	A�B	�B	�B	�B	33B	�B	 �B	!�B	2-B	:^B	H�B	]/B	o�B	�B	�hB	�uB	��B	��B	��B	��B	��B	��B	�B	��B	�!B	�jB	��B	��B	�wB	��B	ĜB	�RB	�-B	�XB	�wB	�qB	�wB	ŢB	��B	��B	��B	�
B	�)B	�5B	�B
VB
uB
�B
�B
�B
oB
1B
1B
	7B
hB
�B
�B
�B
�B
PB
1B
B
1B
%B
oB
�B
oB
DB
JB
VB
B
1B
%B
B	��B

=B
	7B
DB
\B
uB
�B
hB
bB
bB
hB
hB
DB	��B	��B
+B
B
hB
\B
PB
\B
B
+B
JB
	7B
1B
B
%B
B
B
B
%B
B	��B	��B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B
B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
  B
B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B
1B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
JB
DB
JB
JB
JB
JB
JB
PB
PB
PB
PB
PB
VB
\B
bB
bB
bB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
(�B
5?B
9XB
@�B
C�B
H�B
N�B
S�B
YB
^5B
aHB
ffB
k�B
p�B
t�B
w�B
{�B
� B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<�o<#�
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
<e`B<#�
<49X<ě�<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.6 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250332012011312503320120113125033  AO  ARGQ                                                                        20111205113635  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113635  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125033  IP                  G�O�G�O�G�O�                