CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:51Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               ,A   AO  20111130143936  20190522121828  1728_5048_044                   2C  D   APEX                            2142                            040306                          846 @Ԙ)�j�1   @Ԙ*��?�@6`A�7K��c=/��w1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B��B��B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dy` D�	�D�33D���D�ɚD���D�0 D�c3D���D���D�33D�` Dǹ�D�� D�#3D�y�D��3D���D�&fD�Y�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @s33@���@���AffA6ffAVffAvffA�33A�33A�33A�33A�33A�33A�33A�33B33B33B33B��B%��B-��B5��B=��BE��BM��BU��B^  Be��Bm��Bu��B}��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���CffCffCffCffC	ffCffCffCffCffCffCffCffCffCffCffCffC!ffC#ffC%ffC'ffC)ffC+ffC-ffC/ffC1ffC3ffC5ffC7ffC9ffC;ffC=ffC?ffCAffCCffCEffCGffCIffCKffCMffCOffCQffCSffCUffCWffCYffC[ffC]ffC_ffCaffCcffCeffCgffCiffCkffCmffCoffCqffCsffCuffCwffCy� C{ffC}ffCffC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C�� C�� C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C³3Có3Cĳ3Cų3CƳ3Cǳ3Cȳ3Cɳ3Cʳ3C˳3C̳3Cͳ3Cγ3Cϳ3Cг3Cѳ3Cҳ3Cӳ3CԳ3Cճ3Cֳ3C׳3Cس3Cٳ3Cڳ3C۳3Cܳ3Cݳ3C޳3C߳3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C��3C�3C�3C�3C�3C�3C�3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D Y�D ٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚD	Y�D	ٚD
Y�D
ٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�D� DY�DٚD Y�D ٚD!Y�D!ٚD"Y�D"ٚD#Y�D#ٚD$Y�D$ٚD%Y�D%� D&Y�D&ٚD'Y�D'ٚD(Y�D(ٚD)Y�D)ٚD*Y�D*ٚD+Y�D+ٚD,Y�D,ٚD-Y�D-ٚD.Y�D.ٚD/Y�D/ٚD0Y�D0ٚD1Y�D1ٚD2Y�D2ٚD3Y�D3ٚD4Y�D4ٚD5Y�D5ٚD6Y�D6ٚD7Y�D7ٚD8Y�D8ٚD9Y�D9ٚD:Y�D:ٚD;Y�D;ٚD<Y�D<ٚD=Y�D=ٚD>Y�D>ٚD?Y�D?ٚD@Y�D@ٚDAY�DAٚDBY�DBٚDCY�DCٚDDY�DDٚDEY�DEٚDFY�DFٚDGY�DGٚDHY�DHٚDIY�DIٚDJY�DJٚDKY�DKٚDLY�DLٚDMY�DMٚDNY�DNٚDOY�DOٚDPY�DPٚDQY�DQٚDRY�DRٚDSY�DSٚDTY�DTٚDUY�DUٚDVY�DVٚDWY�DWٚDXY�DXٚDYY�DYٚDZY�DZٚD[Y�D[ٚD\Y�D\ٚD]Y�D]ٚD^Y�D^ٚD_Y�D_ٚD`Y�D`ٚDaY�DaٚDbY�DbٚDcY�DcٚDdY�DdٚDeY�DeٚDfY�DfٚDgY�DgٚDhY�DhٚDiY�DiٚDjY�DjٚDkY�DkٚDlY�DlٚDmY�DmٚDnY�DnٚDoY�DoٚDpY�DpٚDqY�DqٚDrY�DrٚDsY�DsٚDtY�DtٚDuY�DuٚDvY�Dy9�D��fD�  D�y�D��fD��D��D�P D��fD��D�  D�L�DǦfD���D� D�ffD� D��D�3D�FfD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AͅA͇+A͉7A͋DA͋DA͍PA͍PA͍PA͍PA͏\A͏\A͑hA͑hA͓uA͕�A͓uA͓uA͗�A͗�A͗�A͙�A͛�A͛�A͛�A͙�A͗�A͗�A͕�A͑hA͕�A̓A�XA��AȬAę�AþwA�dZA���A�  A��wA�|�A���A��A��jA�E�A��/A���A�z�A�-A��9A�;dA�bNA���A�dZA��7A�1'A��mA�I�A��A�5?A���A��A�ȴA�=qA��mA�$�A��jA�/A���A�+A��A�K�A��HA���A�C�A�ĜA���A�`BA��-A�{A��!A���A�l�A�K�A��A���A���A�ffA��A�x�A�  A���A�~�A��A��9A���A��`A��mA�%A��wA��#A�5?A���A�E�A�VA�Q�A��^A�jA�ZA�
=A�ƨA��^A�ffA�\)A�S�A��A�A�C�A�oA�hsA��A��A�A�A��A�p�A��A}Ay��AtM�Aq��Ap~�Ao%Ahn�AbQ�A]�FAY%AV�AQ�AO��AMhsAI|�AF�!ACO�ABbA@�9A?�wA=�mA<ĜA;�A:��A9;dA7��A6�yA5��A5oA4~�A3�
A2��A2A1?}A0��A0 �A/�A/�FA/O�A.5?A.A-;dA,1'A+��A+�7A*z�A)��A(�A( �A'�-A'�A&�+A%��A$�A#�A#O�A#A!�^A�wA�A�+A1'Al�Av�A�A~�A�-A��A�A��Al�AVA�Ap�AĜAA��A�A�jA�FA�A�mA
��A	��A��A��A33A1'AK�AbNAO�A �D@�@�O�@�1@���@�p�@�A�@���@�-@�@���@홚@�1'@�V@�V@�1'@�S�@��@�+@�ff@��@�7L@�R@�-@�V@�K�@�X@�^5@�1@�^5@��#@���@Ցh@ם�@�A�@�  @�dZ@�O�@ҧ�@��@�~�@�p�@�o@ÍP@�(�@�dZ@�r�@�1'@�(�@� �@�ƨ@��H@��@�hs@��@ēu@�I�@�1'@�1@���@�ƨ@Ý�@�"�@�=q@���@�z�@�  @��P@�S�@�dZ@�l�@�K�@��@���@�ff@��-@���@�z�@�1'@��@��m@���@�C�@���@��H@���@�ff@�$�@���@�hs@��@�bN@��@�1'@�1'@��
@�
=@�ff@��h@���@��@�Z@�  @��F@�dZ@�K�@��@���@�V@��@�{@�$�@�5?@�5?@�5?@�-@���@�G�@�(�@�33@�S�@�|�@�-@�E�@���@���@��-@��-@��#@��D@��H@�ȴ@��\@��!@���@���@�"�@�~�@��y@�~�@�%@��@��@�Q�@�%@�(�@�-@�{@�1'@���@���@�
=@�$�@���@�7L@���@��m@�33@�^5@��-@��@�z�@�j@���@��@��H@�"�@���@��@���@���@�n�@�E�@�-@�5?@�v�@��^@�?}@�hs@�?}@�A�@��@���@�ff@�$�@��@���@���@�x�@�O�@��@��@�Ĝ@��@��@��j@� �@�  @���@��@��+@�-@���@���@�p�@�/@��@��@���@���@�j@�9X@��;@��;@��@�9X@�/@���@�E�@��@���@���@��D@�bN@��;@��@��w@��@�  @�  @��@��w@���@�t�@�K�@��@��@�ȴ@�~�@�M�@�M�@�J@���@��@�(�@�Z@�Z@�A�@�1'@�(�@��w@�dZ@�;d@�"�@�@���@�E�@��T@���@�O�@�7L@��@��9@�ƨ@�~�@��-@�/@��9@�b@��@�1'@��@��/@��@�K�@���@z��@s�
@l�@fE�@^5?@Vff@MV@E�-@?��@9��@2~�@-�@(��@$�D@ 1'@��@��@�@��@
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AͅA͇+A͉7A͋DA͋DA͍PA͍PA͍PA͍PA͏\A͏\A͑hA͑hA͓uA͕�A͓uA͓uA͗�A͗�A͗�A͙�A͛�A͛�A͛�A͙�A͗�A͗�A͕�A͑hA͕�A̓A�XA��AȬAę�AþwA�dZA���A�  A��wA�|�A���A��A��jA�E�A��/A���A�z�A�-A��9A�;dA�bNA���A�dZA��7A�1'A��mA�I�A��A�5?A���A��A�ȴA�=qA��mA�$�A��jA�/A���A�+A��A�K�A��HA���A�C�A�ĜA���A�`BA��-A�{A��!A���A�l�A�K�A��A���A���A�ffA��A�x�A�  A���A�~�A��A��9A���A��`A��mA�%A��wA��#A�5?A���A�E�A�VA�Q�A��^A�jA�ZA�
=A�ƨA��^A�ffA�\)A�S�A��A�A�C�A�oA�hsA��A��A�A�A��A�p�A��A}Ay��AtM�Aq��Ap~�Ao%Ahn�AbQ�A]�FAY%AV�AQ�AO��AMhsAI|�AF�!ACO�ABbA@�9A?�wA=�mA<ĜA;�A:��A9;dA7��A6�yA5��A5oA4~�A3�
A2��A2A1?}A0��A0 �A/�A/�FA/O�A.5?A.A-;dA,1'A+��A+�7A*z�A)��A(�A( �A'�-A'�A&�+A%��A$�A#�A#O�A#A!�^A�wA�A�+A1'Al�Av�A�A~�A�-A��A�A��Al�AVA�Ap�AĜAA��A�A�jA�FA�A�mA
��A	��A��A��A33A1'AK�AbNAO�A �D@�@�O�@�1@���@�p�@�A�@���@�-@�@���@홚@�1'@�V@�V@�1'@�S�@��@�+@�ff@��@�7L@�R@�-@�V@�K�@�X@�^5@�1@�^5@��#@���@Ցh@ם�@�A�@�  @�dZ@�O�@ҧ�@��@�~�@�p�@�o@ÍP@�(�@�dZ@�r�@�1'@�(�@� �@�ƨ@��H@��@�hs@��@ēu@�I�@�1'@�1@���@�ƨ@Ý�@�"�@�=q@���@�z�@�  @��P@�S�@�dZ@�l�@�K�@��@���@�ff@��-@���@�z�@�1'@��@��m@���@�C�@���@��H@���@�ff@�$�@���@�hs@��@�bN@��@�1'@�1'@��
@�
=@�ff@��h@���@��@�Z@�  @��F@�dZ@�K�@��@���@�V@��@�{@�$�@�5?@�5?@�5?@�-@���@�G�@�(�@�33@�S�@�|�@�-@�E�@���@���@��-@��-@��#@��D@��H@�ȴ@��\@��!@���@���@�"�@�~�@��y@�~�@�%@��@��@�Q�@�%@�(�@�-@�{@�1'@���@���@�
=@�$�@���@�7L@���@��m@�33@�^5@��-@��@�z�@�j@���@��@��H@�"�@���@��@���@���@�n�@�E�@�-@�5?@�v�@��^@�?}@�hs@�?}@�A�@��@���@�ff@�$�@��@���@���@�x�@�O�@��@��@�Ĝ@��@��@��j@� �@�  @���@��@��+@�-@���@���@�p�@�/@��@��@���@���@�j@�9X@��;@��;@��@�9X@�/@���@�E�@��@���@���@��D@�bN@��;@��@��w@��@�  @�  @��@��w@���@�t�@�K�@��@��@�ȴ@�~�@�M�@�M�@�J@���@��@�(�@�Z@�Z@�A�@�1'@�(�@��w@�dZ@�;d@�"�@�@���@�E�@��T@���@�O�@�7L@��@��9@�ƨ@�~�@��-@�/@��9@�b@��@�1'@��@��/@��@�K�@���@z��@s�
@l�@fE�@^5?@Vff@MV@E�-@?��@9��@2~�@-�@(��@$�D@ 1'@��@��@�@��@
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB}�B}�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B}�B}�B}�B}�B|�B|�B|�B|�B|�B|�Bz�Bv�BjB6FBoB�B+B;dBF�Be`B�%B��B��B��B�BB�B�B�B�B�B�B!�B.BH�BhsBffB|�B�\Br�Bl�Bu�B�B�B}�B}�B|�Bu�Bu�Bz�B� B� B� B�B�B�B�JB�JB�VB�%B�B�B�B� B� B}�Bx�Bu�Bu�Bs�Bn�BhsBcTB_;BZBI�B>wB/B�B1B��B�NB�5B��B��B��B�qB�3B��B�bB�PB�Bl�BH�B6FB$�BbB
��B
�`B
��B
��B
��B
�\B
v�B
e`B
(�B
B	�B	�
B	�FB	��B	�\B	�B	E�B	/B	)�B	\B�;B��B�3B�-B��B�uB�DB�JB�B� B�B� B� B�B�B~�B}�B~�B}�Bz�B}�Bv�Bu�Bu�Bu�Bv�Bw�B}�Bx�B� B~�B�+B�B~�B�%B�Bv�B~�B|�Bw�Bx�Bt�Br�Bu�Bx�B|�Bt�Bt�Bu�Bv�Bv�Bu�Bt�Bo�BhsBhsBgmBffB^5B_;B`BBgmBbNBaHB`BB_;B\)B[#BXBW
BZBT�BW
BYBZBM�BL�BM�BQ�BG�BH�BP�BJ�BA�BH�B@�BH�B?}B?}BE�BA�BG�B@�B<jBL�BI�BA�B6FB5?B5?B5?B49B2-B2-B2-B49B6FB5?B.B1'B0!B33BA�BE�B[#BffBjBjBl�BffBe`B]/B^5BK�BC�BN�BffBq�Br�Bt�Bz�B|�B�B�7B�1B�PB�=B�PB�bB�{B��B��B��B��B��B��B��B��B��B��B�B�?B�XB�jB�jBĜBB��BBBÖBǮBȴBɺB��B��B��B�)B�;B�HB�`B�B�B�B�B��B��B��B��B	B	B	+B	
=B	PB	hB	�B	�B	 �B	&�B	)�B	.B	0!B	2-B	33B	6FB	7LB	8RB	9XB	;dB	9XB	8RB	:^B	?}B	@�B	6FB	33B	2-B	33B	5?B	;dB	8RB	33B	6FB	8RB	7LB	>wB	:^B	49B	YB	_;B	\)B	W
B	H�B	=qB	@�B	K�B	[#B	G�B	A�B	Q�B	]/B	]/B	ZB	XB	YB	XB	XB	VB	R�B	P�B	P�B	S�B	R�B	Q�B	S�B	T�B	ZB	`BB	aHB	cTB	ffB	iyB	jB	m�B	n�B	o�B	u�B	w�B	v�B	x�B	x�B	v�B	t�B	t�B	v�B	y�B	{�B	|�B	}�B	~�B	� B	�B	�B	�%B	�JB	�VB	�bB	�uB	�uB	�{B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�^B	��B	ĜB	ŢB	ĜB	ŢB	ĜB	ŢB	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�5B	�;B	�BB	�HB	�`B	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�mB	�`B	�TB	�NB	�NB	�TB	�ZB	�yB	�B	�B	�yB	�yB	��B
JB
�B
!�B
"�B
)�B
33B
:^B
A�B
C�B
K�B
S�B
ZB
]/B
cTB
hsB
m�B
q�B
t�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B}�B}�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B}�B}�B}�B}�B|�B|�B|�B|�B|�B|�B{�Bw�Bt�BD�B�B�B2-B@�BL�BiyB�7B��B�B��B�NB�B�B��B��B�B��B$�B2-BK�BjBgmB� B��Bu�Bn�Bw�B�B�B� B�B~�Bx�Bx�B}�B�B�B�B�B�%B�1B�PB�VB�oB�=B�B�B�B�B�B� By�Bw�Bw�Bw�Bq�BjBdZBcTBaHBN�BB�B33B�BPB��B�`B�HB��B��B��B��B�^B��B�oB�\B�7By�BO�B<jB)�B�B
��B
�B
��B
ȴB
�!B
��B
~�B
n�B
1'B
B	�B	�;B	�^B	��B	�oB	�bB	R�B	9XB	5?B	�B�B��B�^B�qB��B��B�\B�bB�+B�%B�B�B�B�+B�+B�B�B�B�B}�B�Bz�Bx�Bw�Bx�Bw�Bx�B~�B{�B�B�B�=B�B� B�7B�+Bx�B�B}�By�Bz�Bw�Bv�Bw�Bz�B}�Bx�Bz�Bw�Bx�Bx�Bx�Bw�Bs�Bl�Bk�Bk�BhsBcTBcTBdZBiyBcTBdZBffBcTB]/B^5B[#BZB]/BYBZB\)B^5BT�BO�BP�BS�BI�BJ�BT�BK�BC�BI�BC�BJ�BA�BA�BK�BF�BI�BC�B?}BN�BK�BC�B7LB6FB6FB6FB6FB6FB49B33B7LB9XB:^B2-B49B1'B33BB�BB�B[#BgmBl�Bn�Bp�BiyBjB`BBhsBQ�BB�BI�Be`Br�Bs�Bu�B{�B}�B�%B�=B�7B�VB�DB�VB�hB��B��B��B��B��B��B��B��B��B��B��B�B�FB�^B�qB�qBƨBÖB��BÖBÖBÖBȴBɺB��B��B��B��B�/B�BB�NB�fB�B�B�B�B��B��B��B	  B	B	B	1B	DB	VB	oB	�B	�B	!�B	'�B	+B	.B	0!B	2-B	33B	6FB	7LB	9XB	;dB	=qB	;dB	8RB	:^B	B�B	G�B	7LB	33B	2-B	33B	5?B	>wB	;dB	49B	7LB	8RB	7LB	?}B	9XB	0!B	YB	`BB	^5B	\)B	K�B	=qB	@�B	M�B	^5B	G�B	A�B	Q�B	_;B	_;B	\)B	YB	ZB	YB	ZB	XB	T�B	Q�B	R�B	T�B	S�B	S�B	T�B	VB	ZB	aHB	aHB	dZB	gmB	jB	k�B	n�B	n�B	o�B	w�B	x�B	v�B	y�B	z�B	x�B	u�B	u�B	w�B	z�B	|�B	}�B	~�B	� B	�B	�B	�B	�%B	�JB	�\B	�hB	�{B	�{B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�^B	��B	ŢB	ǮB	ŢB	ƨB	ŢB	ƨB	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	�#B	�)B	�B	�#B	�5B	�BB	�BB	�HB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�fB	�ZB	�TB	�NB	�TB	�ZB	�yB	�B	�B	�B	�yB	��B
JB
�B
!�B
"�B
+B
33B
;dB
A�B
D�B
L�B
S�B
[#B
]/B
cTB
iyB
n�B
q�B
u�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<T��<#�
<49X<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.6 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452082012011014520820120110145208  AO  ARGQ                                                                        20111130143936  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143936  QCF$                G�O�G�O�G�O�4000            UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145208  IP                  G�O�G�O�G�O�                