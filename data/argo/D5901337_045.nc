CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:36Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               -A   AO  20111205113106  20190522121836  1901_5055_045                   2C  D   APEX                            2140                            040306                          846 @ԙ.��?�1   @ԙ/���@/n��O��c�^5?}1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DC��DD� DE  DE� DF  DF� DG  DG� DG��DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dy��D�  D�FfD�\�D��3D�3D�6fD�c3D��fD���D�&fD�Y�D�ɚD�� D�0 D�S3D�ɚD��3D�3D�P D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @Y��@���@���AffA6ffAVffAvffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B33B33B%��B-��B5��B=��BE��BM��BU��B]��Be��Bm��Bu��B}��B���B���B�  B���B���B���B���B���B���B���B�33B�ffB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���CffCffCffCffC	ffCffCffCffCffCffC� CffCffCffC� CffC!ffC#ffC%ffC'ffC)ffC+ffC-ffC/ffC1ffC3ffC5ffC7ffC9ffC;ffC=ffC?ffCAffCCL�CEffCGffCIffCKffCM� COffCQffCSffCUffCWffCYffC[ffC]ffC_ffCaffCcffCeffCgffCiffCkffCmffCoffCqffCsffCuffCwffCyffC{ffC}ffCffC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�� C��3C��3C��3C��3C��3C��3C��3C��3C�� C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C³3Có3Cĳ3Cų3CƳ3Cǳ3Cȳ3Cɳ3Cʳ3C˳3C̳3Cͳ3Cγ3Cϳ3Cг3Cѳ3Cҳ3Cӳ3CԳ3Cճ3Cֳ3C׳3Cس3Cٳ3Cڳ3C۳3Cܳ3Cݳ3C޳3C߳3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C��3C�3C�3C�3C�3C�3C�3C�� C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D Y�D ٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚD	Y�D	ٚD
Y�D
ٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚD Y�D ٚD!Y�D!ٚD"Y�D"ٚD#Y�D#ٚD$Y�D$ٚD%Y�D%ٚD&Y�D&ٚD'Y�D'ٚD(Y�D(ٚD)Y�D)ٚD*Y�D*ٚD+Y�D+ٚD,Y�D,ٚD-Y�D-ٚD.Y�D.ٚD/Y�D/ٚD0Y�D0ٚD1Y�D1ٚD2Y�D2ٚD3Y�D3ٚD4Y�D4ٚD5Y�D5ٚD6Y�D6ٚD7Y�D7ٚD8Y�D8ٚD9Y�D9ٚD:Y�D:ٚD;Y�D;ٚD<Y�D<ٚD=Y�D=ٚD>Y�D>ٚD?Y�D?ٚD@Y�D@ٚDAY�DAٚDBY�DBٚDCY�DC�3DDY�DDٚDEY�DEٚDFY�DFٚDGY�DG�3DHY�DH� DIY�DIٚDJY�DJٚDKY�DKٚDLY�DLٚDMY�DMٚDNY�DNٚDOY�DOٚDPY�DPٚDQY�DQٚDRY�DRٚDSY�DSٚDTY�DTٚDUY�DUٚDVY�DVٚDWY�DWٚDXY�DXٚDYY�DYٚDZY�DZٚD[Y�D[ٚD\Y�D\ٚD]Y�D]ٚD^Y�D^ٚD_Y�D_ٚD`Y�D`ٚDaY�DaٚDbY�DbٚDcY�DcٚDdY�DdٚDeY�DeٚDfY�DfٚDgY�DgٚDhY�DhٚDiY�DiٚDjY�DjٚDkY�DkٚDlY�DlٚDmY�DmٚDnY�DnٚDoY�DoٚDpY�DpٚDqY�DqٚDrY�DrٚDsY�DsٚDtY�DtٚDuY�DuٚDvY�DvٚDys3D���D�33D�I�D�� D�  D�#3D�P D��3D��fD�3D�FfDǶfD���D��D�@ D�fD�� D�  D�<�D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A��A���A���A���A�%A�A�A�A�  A���A�JA�oA�1A���A�%A�ƨA�9XA��`A��TA��/A���Aԏ\A�l�A���A�  A�;dA�I�A�A�A�O�A̾wA�E�A��;A���A�G�A�K�A�bA�7LA��HA�XA�+A�JA��/A�A�A��9A�ZA�p�A���A��A�I�A��#A��#A���A�x�A��uA���A��A��A�-A�n�A��A�l�A���A�E�A��A�p�A�|�A�`BA�K�A�A��;A��HA��A�{A���A�r�A���A��wA��A�ȴA���A�oA�1A��A���A���A���A~ZAy\)Au��Ap��Am��Aj��Ah�uAe�wAb�!A`VA]�FA\E�AZĜAXM�AS\)AO�
AM`BAK\)AJ$�AH��AF�AD�9AB�uA@5?A=�wA<M�A9A6A41A3`BA2v�A1��A0�RA/�A-��A,VA+�FA+t�A+�A+�FA+��A,$�A,^5A,�9A-`BA/K�A/ƨA/��A/&�A.��A.{A.A-�TA-�wA,��A,5?A+l�A*ĜA*ffA(�HA(JA(A'�mA&��A%XA$E�A#VA"A�A!K�A ��A 1'A A�-A&�A�+A�#AO�AS�A�A�+Ar�A(�Ax�A�9A;dA�wA�A�Ap�AȴA�\AQ�AJA�;A�PA�AE�AS�A��A�`Av�A�FAx�AȴA�\A^5A�A�^A�FA��A��AQ�A�PA�9A{A�;A&�A/A|�A
�9A	�mA
A
1'A
�A	��A	oA	x�A��A��AE�A1A�FA��A^5AA�A(�A��A33A��A��A�AjA�mAp�A ��A v�@���@��@�{@���@��F@�ȴ@�~�@�/@���@��j@��@���@�V@���@��`@�Z@�@�~�@�-@�@�h@�r�@�ƨ@�R@�x�@���@��/@�(�@�|�@��y@�?}@��@���@�Ĝ@�ƨ@�^5@噚@��@�D@�\)@�
=@�!@�J@�@�7L@��u@��
@�dZ@�@އ+@�^5@�=q@�$�@�J@ݑh@݁@�hs@�`B@�G�@�%@ܬ@ۮ@���@�-@��@��@�&�@؛�@�  @׍P@���@�V@��#@�V@ӕ�@���@�ff@�5?@�{@���@�b@�1@ЋD@ЋD@�ff@�/@�r�@˾w@�S�@�n�@��@ɺ^@�O�@��@���@ȃ@�I�@�dZ@��y@Ə\@�{@���@ũ�@�O�@���@�9X@ÍP@�C�@��@�v�@��@�=q@���@�x�@���@��@�j@���@���@�dZ@��!@�{@��h@�&�@��@�9X@�t�@�33@�33@���@���@�J@��@�p�@��j@�I�@��w@�S�@���@���@�ff@�-@��-@�G�@�/@�%@���@�bN@���@��w@��P@�+@���@���@�=q@�@���@���@���@��@�9X@�1@��@��y@���@�ff@��7@��@�X@�Ĝ@�Z@��;@���@�|�@�l�@�;d@�@��\@�^5@�5?@��@�@�p�@�/@��/@���@�z�@���@�+@�ȴ@�ȴ@���@��R@��+@�V@�{@��#@��h@�p�@�7L@���@���@��u@�Q�@�  @��@�C�@��@���@�E�@��-@�hs@�O�@��@���@��9@��@�1'@��
@���@��@�;d@�n�@�-@��@���@�hs@���@���@�r�@�Z@� �@�ƨ@��P@�t�@�o@���@�=q@���@���@���@�p�@�G�@�V@�bN@���@��F@��P@�+@���@�ȴ@�n�@��#@�x�@���@�r�@�Q�@��F@�x�@��m@��@zn�@n@c��@[��@S�F@MV@Gl�@?�;@7K�@01'@)��@"�H@E�@X@��@=q@E�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A��A���A���A���A�%A�A�A�A�  A���A�JA�oA�1A���A�%A�ƨA�9XA��`A��TA��/A���Aԏ\A�l�A���A�  A�;dA�I�A�A�A�O�A̾wA�E�A��;A���A�G�A�K�A�bA�7LA��HA�XA�+A�JA��/A�A�A��9A�ZA�p�A���A��A�I�A��#A��#A���A�x�A��uA���A��A��A�-A�n�A��A�l�A���A�E�A��A�p�A�|�A�`BA�K�A�A��;A��HA��A�{A���A�r�A���A��wA��A�ȴA���A�oA�1A��A���A���A���A~ZAy\)Au��Ap��Am��Aj��Ah�uAe�wAb�!A`VA]�FA\E�AZĜAXM�AS\)AO�
AM`BAK\)AJ$�AH��AF�AD�9AB�uA@5?A=�wA<M�A9A6A41A3`BA2v�A1��A0�RA/�A-��A,VA+�FA+t�A+�A+�FA+��A,$�A,^5A,�9A-`BA/K�A/ƨA/��A/&�A.��A.{A.A-�TA-�wA,��A,5?A+l�A*ĜA*ffA(�HA(JA(A'�mA&��A%XA$E�A#VA"A�A!K�A ��A 1'A A�-A&�A�+A�#AO�AS�A�A�+Ar�A(�Ax�A�9A;dA�wA�A�Ap�AȴA�\AQ�AJA�;A�PA�AE�AS�A��A�`Av�A�FAx�AȴA�\A^5A�A�^A�FA��A��AQ�A�PA�9A{A�;A&�A/A|�A
�9A	�mA
A
1'A
�A	��A	oA	x�A��A��AE�A1A�FA��A^5AA�A(�A��A33A��A��A�AjA�mAp�A ��A v�@���@��@�{@���@��F@�ȴ@�~�@�/@���@��j@��@���@�V@���@��`@�Z@�@�~�@�-@�@�h@�r�@�ƨ@�R@�x�@���@��/@�(�@�|�@��y@�?}@��@���@�Ĝ@�ƨ@�^5@噚@��@�D@�\)@�
=@�!@�J@�@�7L@��u@��
@�dZ@�@އ+@�^5@�=q@�$�@�J@ݑh@݁@�hs@�`B@�G�@�%@ܬ@ۮ@���@�-@��@��@�&�@؛�@�  @׍P@���@�V@��#@�V@ӕ�@���@�ff@�5?@�{@���@�b@�1@ЋD@ЋD@�ff@�/@�r�@˾w@�S�@�n�@��@ɺ^@�O�@��@���@ȃ@�I�@�dZ@��y@Ə\@�{@���@ũ�@�O�@���@�9X@ÍP@�C�@��@�v�@��@�=q@���@�x�@���@��@�j@���@���@�dZ@��!@�{@��h@�&�@��@�9X@�t�@�33@�33@���@���@�J@��@�p�@��j@�I�@��w@�S�@���@���@�ff@�-@��-@�G�@�/@�%@���@�bN@���@��w@��P@�+@���@���@�=q@�@���@���@���@��@�9X@�1@��@��y@���@�ff@��7@��@�X@�Ĝ@�Z@��;@���@�|�@�l�@�;d@�@��\@�^5@�5?@��@�@�p�@�/@��/@���@�z�@���@�+@�ȴ@�ȴ@���@��R@��+@�V@�{@��#@��h@�p�@�7L@���@���@��u@�Q�@�  @��@�C�@��@���@�E�@��-@�hs@�O�@��@���@��9@��@�1'@��
@���@��@�;d@�n�@�-@��@���@�hs@���@���@�r�@�Z@� �@�ƨ@��P@�t�@�o@���@�=q@���@���@���@�p�@�G�@�V@�bN@���@��F@��P@�+@���@�ȴ@�n�@��#@�x�@���@�r�@�Q�@��F@�x�@��m@��@zn�@n@c��@[��@S�F@MV@Gl�@?�;@7K�@01'@)��@"�H@E�@X@��@=q@E�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�1B�1B�+B�+B�+B�+B�+B�+B�=B�7B�+B�+B�+B�+B�7B�JB��B��B	��B
w�B
��BDB�B�B �B%�B%�B�B
��B
�#B
�5B
��BBB+B\B
��B
��B
��B
�XB
�B
��BB
�B
��B
��Bp�B��B�jB1B9XB]/BiyB�VB�{B��B��B��B��B��B��B��B��B�\B�DB�Bq�Bk�B`BBW
BO�BG�B8RB#�BVB�NB��B�^B��Br�B=qB#�BB
��B
��B
�B
�7B
dZB
2-B
JB	��B	�B	�{B	|�B	iyB	YB	J�B	8RB	'�B	�B	\B	1B��B�B�B��BȴBȴBƨBŢBƨBBĜBƨB�dB�9B�-B�B�3B�wBB��B��B��B��B�B�#B�ZB��B	�B	>wB	K�B	YB	k�B	�1B	��B	�TB	�B	�B	��B
B
PB
VB
bB
�B
!�B
�B
�B
'�B
"�B
!�B
"�B
"�B
�B
bB
+B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
+B
�B
�B
�B
oB
PB
1B
  B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�yB	�yB	�yB	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
B
B
%B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
hB
VB
DB
	7B
%B
B
%B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�yB	�B	�yB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
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
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
+B
+B
+B
+B
+B
%B
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
	7B
	7B

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
DB
DB
DB
DB
JB
PB
VB
VB
PB
PB
PB
VB
VB
VB
VB
VB
VB
\B
bB
bB
bB
bB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
$�B
)�B
0!B
5?B
<jB
@�B
F�B
L�B
P�B
T�B
ZB
aHB
e`B
jB
o�B
s�B
w�B
y�B
}�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�1B�1B�+B�+B�+B�+B�+B�+B�=B�7B�+B�+B�+B�+B�7B�JB��B��B	��B
}�B
��BDB�B�B!�B&�B'�B �B
��B
�;B
�NB  BBB1BhB
��B
�/B
��B
�jB
�5B
��B+B
�B
��B
��Bo�B�)B�jBbB=qB`BBl�B�{B��B��B��B��B��B��B��B��B��B�oB�PB�7Bt�Bp�Be`B\)BVBO�BB�B)�B�B�B��B��B��B� BD�B)�BoB
�/B
ŢB
�FB
�hB
p�B
:^B
�B	�;B	�LB	��B	�B	q�B	_;B	S�B	A�B	0!B	#�B	{B	PB	B��B�NB��B��B��B��B��B��BȴB��B��B�}B�dB�jB�9B�?B��BŢB��B��B��B��B�)B�)B�ZB��B	�B	>wB	K�B	YB	jB	�B	��B	�ZB	�B	��B	��B
B
VB
\B
uB
�B
#�B
 �B
 �B
-B
%�B
!�B
#�B
&�B
�B
{B
DB
B	��B	��B	�B	�B	�B	�B	�B	�B	��B	�B	��B
B
�B
�B
�B
�B
uB
VB
B
  B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	�B	��B
B	��B	��B	��B
%B
+B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
hB
PB
DB
1B
+B
1B
%B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B
B
  B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
+B
1B
	7B
1B
1B
1B
+B
1B
+B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B
DB

=B
	7B

=B
DB
DB
JB
JB
JB
JB
JB
JB
JB
JB
DB
JB
JB
PB
VB
\B
\B
VB
VB
VB
\B
\B
\B
\B
\B
\B
bB
hB
hB
hB
oB
uB
uB
uB
{B
{B
{B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
$�B
)�B
0!B
5?B
<jB
A�B
F�B
L�B
P�B
VB
ZB
aHB
ffB
k�B
o�B
s�B
w�B
z�B
}�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<T��<#�
<#�
<T��<#�
<#�
<#�
<#�
<D��<#�
<e`B<49X<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.6 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250232012011312502320120113125023  AO  ARGQ                                                                        20111205113106  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113106  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125023  IP                  G�O�G�O�G�O�                