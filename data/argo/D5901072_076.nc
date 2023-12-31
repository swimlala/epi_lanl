CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:59Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               LA   AO  20111130144303  20190522121829  1728_5048_076                   2C  D   APEX                            2142                            040306                          846 @����
1   @�眕ο�@6^�Q��b�n��1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!y�D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(�fD)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dc��Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwl�Dy� D�  D�9�D�s3D�� D���D�)�D�Y�D��3D�� D�33D�Y�Dǹ�D�� D�@ D�vfD���D��fD�)�D�` D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @`  @���@���AffA6ffAVffAvffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B  B%��B-��B5��B=��BE��BM��BU��B]��Be��Bm��Bu��B}��B���B���B�  B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���CffCffCffCffC	ffCffCffCffCffCffCffCffCffCffCffCffC!ffC#ffC%ffC'ffC)ffC+ffC-ffC/ffC1ffC3ffC5ffC7ffC9ffC;ffC=ffC?ffCAffCCffCEffCGffCIffCKffCMffCOffCQffCSffCUffCWffCYffC[ffC]ffC_ffCaffCcffCeffCgffCiffCkffCmffCoffCqffCsffCuffCwffCyffC{ffC}ffCffC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�� C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C³3Có3Cĳ3Cų3CƳ3Cǳ3Cȳ3Cɳ3Cʳ3C˳3C̦fCͳ3Cγ3Cϳ3Cг3Cѳ3Cҳ3Cӳ3CԳ3C�� Cֳ3C׳3Cس3Cٳ3Cڳ3C۳3Cܳ3Cݳ3C޳3C߳3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C��3C�3C�fC�3C�3C�3C�3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D Y�D ٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚD	Y�D	ٚD
Y�D
ٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚDY�DٚD Y�D ٚD!S3D!ٚD"Y�D"ٚD#Y�D#ٚD$Y�D$ٚD%Y�D%ٚD&Y�D&ٚD'Y�D'ٚD(` D(ٚD)Y�D)ٚD*Y�D*ٚD+Y�D+ٚD,Y�D,ٚD-Y�D-ٚD.Y�D.ٚD/Y�D/ٚD0Y�D0ٚD1Y�D1ٚD2Y�D2ٚD3Y�D3ٚD4Y�D4ٚD5Y�D5ٚD6Y�D6ٚD7Y�D7ٚD8Y�D8ٚD9Y�D9ٚD:Y�D:ٚD;Y�D;ٚD<Y�D<ٚD=Y�D=ٚD>Y�D>ٚD?Y�D?ٚD@Y�D@ٚDAY�DAٚDBY�DBٚDCY�DCٚDDY�DDٚDEY�DEٚDFY�DFٚDGY�DGٚDHY�DHٚDIY�DIٚDJY�DJٚDKY�DKٚDLY�DLٚDMY�DMٚDNY�DNٚDOY�DOٚDPY�DPٚDQY�DQٚDRY�DRٚDSY�DSٚDTY�DTٚDUY�DUٚDVY�DVٚDWY�DWٚDXY�DXٚDYY�DYٚDZY�DZٚD[Y�D[ٚD\Y�D\ٚD]Y�D]ٚD^Y�D^ٚD_Y�D_ٚD`Y�D`ٚDaY�DaٚDbY�DbٚDcY�Dc�3DdY�DdٚDeY�DeٚDfY�DfٚDgY�DgٚDhY�DhٚDiY�DiٚDjY�DjٚDkY�DkٚDlY�DlٚDmY�DmٚDn` DnٚDoY�DoٚDpY�DpٚDqY�DqٚDrY�DrٚDsY�DsٚDtY�DtٚDuY�DuٚDvY�DvٚDwFfDyy�D���D�&fD�` D���D�ٚD�fD�FfD�� D���D�  D�FfDǦfD���D�,�D�c3D���D��3D�fD�L�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�9XA�E�A�I�A�I�A�M�A�M�A�M�A�M�A�O�A�O�A�S�A�VA�VA�XA�bNA���A˥�Aˇ+A�bNA�VA�/A�VA�dZA�ffA�S�A��HA�n�A�9XA�"�A���A�|�A�1A�=qA���A��-A�
=A��jA��A���A�~�A��A�ȴA�;dA��A��A�C�A��;A���A���A�-A���A�A��hA�VA�jA��DA�z�A�bNA�M�A�/A��TA�n�A��HA�E�A��A�/A���A�S�A��TA�E�A�jA���A�
=A�C�A�`BA�{A�%A���A���A�^5A�1A��wA�M�A�S�A�VA�ZA�9XA�
=A�;dA�&�A�O�A�?}A�dZA��HA�%A�-A�oA��A�=qA�A��
A�33A��A��A�K�A�"�A�l�A�%A��;A�O�A}l�Ax�jAu�FArAp(�Ao�PAk�TAh��Ae�Aa��A`=qA_��A_�A\jAZ�9AZbNAY��AY�hAX9XAV�AV-AT�RAS��AS�AR$�AP�!APAOdZANv�AMVAK�AI?}AHffAF�/AD�yAC�ABA@z�A>�`A=dZA;hsA:I�A8��A8  A6��A6E�A4v�A1�FA/�7A.I�A-O�A,JA*�A)�A(��A(Q�A'��A&�jA&�A%��A%�hA%C�A$�9A$M�A$JA#��A#33A"�\A!�#A!\)A ��AhsA�A�mA%A~�A5?Ax�A^5AVA{At�A��A�\A^5A��A�A��AM�A��A?}A��AVA��A��A�#AO�A
�A
ĜA
�A
1'A	�^A	`BA��Az�A��A�A�A9XAO�A
=A��A �\@�C�@�?}@�1@�n�@��9@�K�@��@�S�@���@��;@�\@���@�dZ@�v�@���@�F@�{@���@���@�&�@�A�@�;d@�&�@�z�@�A�@�{@��@׍P@�;d@��@��@֏\@�Q�@҇+@�`B@�l�@�9X@��@�/@�Ĝ@�\)@őh@ċD@�  @�l�@�M�@�/@��m@�|�@�@��@��/@��w@��y@���@�7L@��
@��@�t�@�33@���@�5?@�$�@�V@��#@��@���@��/@�I�@��F@�C�@�@�=q@���@�hs@�x�@�x�@�p�@�X@�/@��D@�;d@��@��7@�X@�O�@�?}@�7L@��@�%@��/@�Ĝ@��@�z�@�Z@�9X@�b@���@�
=@��@�5?@��#@���@�Ĝ@�O�@�7L@���@�+@���@���@��@��/@��@�;d@��@���@���@�$�@�p�@�O�@�G�@��@���@�Z@���@�t�@��y@��^@�O�@�V@��/@��j@���@�ff@�p�@��@��;@�l�@�S�@�S�@�S�@�
=@��!@�^5@�J@��@���@��@�7L@���@��9@��u@�r�@�Z@��;@�\)@�33@�o@��@�v�@�5?@�-@��@�@��T@��^@�hs@�7L@���@��9@�1'@�  @��m@��;@��
@���@��w@���@��m@��w@�dZ@�"�@��y@��R@�ff@��T@�@���@��@�p�@�?}@�&�@��@�%@��/@���@��@�j@�j@�r�@�z�@�z�@�r�@�bN@�Z@�Q�@�A�@�1@�ƨ@���@�t�@�;d@�+@�
=@��H@���@�M�@��T@���@�p�@�7L@���@��j@��@�1'@��@���@��w@���@�dZ@���@��@���@���@��+@�v�@�=q@�{@��@��-@��h@��7@��@��@�x�@�p�@�?}@��`@��@�bN@��@�ƨ@���@��P@�t�@�;d@�o@���@���@���@�v�@�^5@�@�p�@�G�@�/@��@��@��@��9@�z�@��@~v�@v��@n�+@g�@^5?@V�y@PbN@H��@A��@;��@5V@.�y@';d@"�\@
=@I�@b@�m@l�@ƨ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�9XA�E�A�I�A�I�A�M�A�M�A�M�A�M�A�O�A�O�A�S�A�VA�VA�XA�bNA���A˥�Aˇ+A�bNA�VA�/A�VA�dZA�ffA�S�A��HA�n�A�9XA�"�A���A�|�A�1A�=qA���A��-A�
=A��jA��A���A�~�A��A�ȴA�;dA��A��A�C�A��;A���A���A�-A���A�A��hA�VA�jA��DA�z�A�bNA�M�A�/A��TA�n�A��HA�E�A��A�/A���A�S�A��TA�E�A�jA���A�
=A�C�A�`BA�{A�%A���A���A�^5A�1A��wA�M�A�S�A�VA�ZA�9XA�
=A�;dA�&�A�O�A�?}A�dZA��HA�%A�-A�oA��A�=qA�A��
A�33A��A��A�K�A�"�A�l�A�%A��;A�O�A}l�Ax�jAu�FArAp(�Ao�PAk�TAh��Ae�Aa��A`=qA_��A_�A\jAZ�9AZbNAY��AY�hAX9XAV�AV-AT�RAS��AS�AR$�AP�!APAOdZANv�AMVAK�AI?}AHffAF�/AD�yAC�ABA@z�A>�`A=dZA;hsA:I�A8��A8  A6��A6E�A4v�A1�FA/�7A.I�A-O�A,JA*�A)�A(��A(Q�A'��A&�jA&�A%��A%�hA%C�A$�9A$M�A$JA#��A#33A"�\A!�#A!\)A ��AhsA�A�mA%A~�A5?Ax�A^5AVA{At�A��A�\A^5A��A�A��AM�A��A?}A��AVA��A��A�#AO�A
�A
ĜA
�A
1'A	�^A	`BA��Az�A��A�A�A9XAO�A
=A��A �\@�C�@�?}@�1@�n�@��9@�K�@��@�S�@���@��;@�\@���@�dZ@�v�@���@�F@�{@���@���@�&�@�A�@�;d@�&�@�z�@�A�@�{@��@׍P@�;d@��@��@֏\@�Q�@҇+@�`B@�l�@�9X@��@�/@�Ĝ@�\)@őh@ċD@�  @�l�@�M�@�/@��m@�|�@�@��@��/@��w@��y@���@�7L@��
@��@�t�@�33@���@�5?@�$�@�V@��#@��@���@��/@�I�@��F@�C�@�@�=q@���@�hs@�x�@�x�@�p�@�X@�/@��D@�;d@��@��7@�X@�O�@�?}@�7L@��@�%@��/@�Ĝ@��@�z�@�Z@�9X@�b@���@�
=@��@�5?@��#@���@�Ĝ@�O�@�7L@���@�+@���@���@��@��/@��@�;d@��@���@���@�$�@�p�@�O�@�G�@��@���@�Z@���@�t�@��y@��^@�O�@�V@��/@��j@���@�ff@�p�@��@��;@�l�@�S�@�S�@�S�@�
=@��!@�^5@�J@��@���@��@�7L@���@��9@��u@�r�@�Z@��;@�\)@�33@�o@��@�v�@�5?@�-@��@�@��T@��^@�hs@�7L@���@��9@�1'@�  @��m@��;@��
@���@��w@���@��m@��w@�dZ@�"�@��y@��R@�ff@��T@�@���@��@�p�@�?}@�&�@��@�%@��/@���@��@�j@�j@�r�@�z�@�z�@�r�@�bN@�Z@�Q�@�A�@�1@�ƨ@���@�t�@�;d@�+@�
=@��H@���@�M�@��T@���@�p�@�7L@���@��j@��@�1'@��@���@��w@���@�dZ@���@��@���@���@��+@�v�@�=q@�{@��@��-@��h@��7@��@��@�x�@�p�@�?}@��`@��@�bN@��@�ƨ@���@��P@�t�@�;d@�o@���@���@���@�v�@�^5@�@�p�@�G�@�/@��@��@��@��9@�z�@��@~v�@v��@n�+@g�@^5?@V�y@PbN@H��@A��@;��@5V@.�y@';d@"�\@
=@I�@b@�m@l�@ƨ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBn�Bo�Bo�Bo�Bp�Bp�Bq�Bq�Bq�Bq�Bs�Bt�Bt�Bv�B~�BbB�B�B��B  BB��B�B�B�B��BPB\BVBPBDBB  BB%BhB�B�B{B1BB��B��B��B��BB
=BB%B
=B1B{B�B�BB
=B.B0!B0!B1'B<jBB�B?}B=qBB�B8RB0!B1'B#�B!�B�BB��B��B�/B��BĜB�B��B�bB�%B�Br�BW
B"�B�B{BbB��B�B�B��B�B��B�VB~�BjBS�BE�B:^B)�B
��B
�B
�jB
�FB
�B
��B
��B
� B
dZB
9XB
�B
B	��B	�B	�ZB	ȴB	��B	�bB	�B	x�B	x�B	y�B	s�B	dZB	aHB	\)B	\)B	S�B	[#B	]/B	R�B	N�B	L�B	I�B	@�B	<jB	:^B	1'B	'�B	 �B	\B	JB	DB	B��B�B�B�mB�HB�5B�B��B��B��BɺB�FB�B�wB�9B�dB��B�oB�PB�DB�%B�7B�+B�B�B�B�B�B�B�B� B}�B|�B{�Bx�Bw�Bw�Bw�Bu�Bv�Bt�Bs�Bv�Bv�By�Bn�Bm�Bk�BiyBhsBgmBe`BcTBaHB`BB]/B\)BYBP�BO�BS�BQ�BQ�BP�BO�BP�BP�BO�BQ�BL�BO�BJ�BK�BP�BD�BF�BD�BA�B:^B;dB=qB@�B8RB5?B6FB<jB9XB,B)�B)�B-B,B(�B$�B&�B%�B&�B$�B"�B"�B"�B �B�B!�B#�B&�B'�B'�B(�B(�B0!B33B0!B8RBA�B6FB7LB7LB:^B<jB@�BA�BC�BD�BF�BI�BK�BM�BO�BR�BT�BXB\)B\)B\)B]/B]/B_;BbNBffBjBm�Bu�Bw�Bw�Bx�B{�By�By�Bz�B� B�B�B�B�1B�7B�7B�=B�PB�uB��B��B�B�B�B�!B�'B�-B�^B�wB��BĜBƨB��B��B��B�B�/B�mB�B�B�B��B��B	  B��B��B��B��B	%B	VB	�B	�B	�B	"�B	$�B	$�B	$�B	'�B	+B	,B	/B	49B	8RB	<jB	8RB	49B	49B	5?B	7LB	=qB	=qB	7LB	6FB	7LB	8RB	7LB	8RB	9XB	;dB	<jB	>wB	B�B	E�B	F�B	I�B	N�B	S�B	XB	ZB	^5B	bNB	gmB	k�B	m�B	m�B	o�B	p�B	r�B	t�B	w�B	x�B	y�B	z�B	}�B	�B	�B	�1B	�=B	�=B	�DB	�PB	�VB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�?B	�?B	�FB	�XB	�^B	�dB	�jB	�qB	�qB	�wB	�wB	��B	ÖB	ĜB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�#B	�)B	�/B	�5B	�BB	�HB	�HB	�NB	�TB	�TB	�fB	�fB	�fB	�mB	�fB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
B
	7B
uB
�B
&�B
,B
2-B
;dB
B�B
F�B
L�B
S�B
[#B
_;B
dZB
ffB
jB
n�B
s�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bn�Bo�Bo�Bo�Bp�Bp�Bq�Bq�Bq�Bq�Bs�Bt�Bt�Bv�B|�BPB�B�B��BBBB�B�B��BB�B{BbB\BoB	7BB%B	7BoB�B!�B�B
=BB��B��B��B��BBbB+B
=BPBDB�B%�B �B	7BDB/B1'B1'B33B>wBD�BB�B@�BG�B:^B2-B33B&�B%�B�B1B��B��B�;B�B��B�B��B�oB�1B�%Bw�BcTB&�B�B�B�BB�B�#BŢB�'B��B�uB�Bp�BXBH�B@�B9XB+B
�BB
�wB
�LB
�9B
�!B
��B
�=B
t�B
G�B
$�B
VB	��B	�B	�B	��B	�'B	��B	�+B	y�B	z�B	�B	x�B	e`B	cTB	]/B	`BB	XB	^5B	bNB	VB	Q�B	P�B	N�B	B�B	>wB	>wB	6FB	-B	'�B	oB	hB	oB	DB��B�B�B�B�sB�NB�#B��B��B��B��B�jB�9B��B�LB�wB��B��B�\B�PB�1B�JB�7B�%B�B�%B�B�B�B�B�B� B� B}�B{�B{�Bz�By�Bx�Bx�Bv�Bv�Bz�B{�B}�Bp�Bo�Bm�BjBk�BiyBgmBdZBcTBcTBbNBaHB_;BXBR�BVBS�BR�BQ�BQ�BR�BR�BQ�BR�BN�BR�BM�BO�BS�BE�BJ�BH�BD�B=qB=qB@�BC�B:^B8RB:^B@�B;dB.B-B-B/B.B-B'�B(�B)�B)�B&�B$�B%�B#�B!�B"�B$�B$�B'�B(�B(�B)�B-B33B5?B49B>wBE�B8RB8RB:^B=qB>wBA�BC�BE�BF�BI�BJ�BL�BP�BQ�BT�BW
BZB^5B_;B]/B^5B^5B`BBcTBgmBjBn�Bv�Bx�Bx�By�B|�Bz�Bz�B{�B�B�B�B�B�7B�=B�=B�DB�\B��B��B�B�B�!B�B�'B�-B�3B�dB�}BBŢBǮB��B��B�B�B�5B�sB�B�B�B��B	  B	B	B��B��B�B	B	VB	�B	 �B	 �B	#�B	%�B	%�B	$�B	(�B	,B	-B	0!B	5?B	9XB	>wB	9XB	5?B	5?B	6FB	9XB	@�B	?}B	9XB	7LB	8RB	9XB	7LB	8RB	:^B	<jB	=qB	?}B	C�B	F�B	G�B	J�B	O�B	T�B	YB	[#B	_;B	cTB	hsB	l�B	n�B	n�B	p�B	q�B	r�B	t�B	x�B	y�B	z�B	{�B	~�B	�B	�%B	�7B	�DB	�DB	�DB	�PB	�VB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�?B	�?B	�LB	�^B	�dB	�jB	�qB	�qB	�wB	�}B	�}B	��B	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�)B	�/B	�5B	�;B	�HB	�NB	�NB	�NB	�ZB	�ZB	�mB	�mB	�mB	�sB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
B
	7B
{B
�B
&�B
-B
33B
;dB
B�B
G�B
M�B
S�B
\)B
_;B
dZB
gmB
k�B
n�B
s�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<u<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.6 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452192012011014521920120110145220  AO  ARGQ                                                                        20111130144303  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144303  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145220  IP                  G�O�G�O�G�O�                