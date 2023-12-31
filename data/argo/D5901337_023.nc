CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:29Z UW 3.1 conversion   
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
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112721  20190522121836  1901_5055_023                   2C  D   APEX                            2140                            040306                          846 @�b���01   @�b��_�@.8Q���cRI�^51   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�33A�33B   BffB  B  B   B(  B0  B7��B?��BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B���B�  B�33B�  B���C�fC  C�C  C
  C�C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(�C*�C,�C.  C/�fC2  C4  C6  C7�fC:  C<  C>  C@�CB�CD  CF�CH  CJ  CL  CN  CP�CR  CT  CV  CX�CZ�C\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl�Cn  Cp  Cr�Ct  Cv  Cx�Cz  C|  C~  C��C�  C��3C�  C��C��C�  C��3C�  C�  C��C�  C��3C�  C�  C��C�  C��3C�  C�  C��C�  C��3C��3C�  C�  C��C�  C�  C��C��C��C�  C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C�  C��C�  C��3C�  C��C��C�  C��3C��3C�  C��C�  C�  C��3C��3C��3C�  C��C��C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C��3C��3C��3C��3C��3C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  Dy�D  D�fDfD� D��Dy�D��D� DfD�fDfD�fD	fD	� D	��D
y�D
��Dy�D  D� D  D� D  D�fD  Dy�D��D� D  D�fDfD�fDfD� D��D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� DfD� D��D� DfD� D   D y�D ��D!� D"fD"� D#  D#� D$  D$�fD%  D%� D&fD&� D'  D'� D'��D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/fD/� D0  D0� D1  D1� D1��D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8fD8� D8��D9y�D:  D:� D;  D;y�D<  D<� D=fD=� D=��D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DFfDF�fDG  DGy�DH  DH� DH��DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP�fDQ  DQ� DR  DR�fDSfDS� DT  DTy�DU  DU�fDV  DV� DW  DWy�DX  DX�fDY  DY�fDZ  DZ� D[  D[� D[��D\y�D]  D]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Dby�Dc  Dc� Dc��Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Dh��Di� DjfDj�fDk  Dk� Dl  Dl�fDm  Dmy�Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dy��D�fD�33D�� D�� D��fD�FfD��3D���D�fD�,�D���Dǣ3D��fD�#3D�p Dਗ਼D���D�#3D�<�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@`  @�  @�  A  A8  AX  Ax  A�  A�  A�  A���A�  A�33A�33A�  BffB  B  B  B&  B.  B5��B=��BF  BN  BV  B^  Be��Bn  Bv  B~  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B���B�  B�33B�  B���CffC� C��C� C	� C��C� C� C� C� C� C� C� C� C��C� C!� C#� C%� C'��C)��C+��C-� C/ffC1� C3� C5� C7ffC9� C;� C=� C?��CA��CC� CE��CG� CI� CK� CM� CO��CQ� CS� CU� CW��CY��C[� C]� C_� Ca� Cc� Ce� CgffCi� Ck��Cm� Co� Cq��Cs� Cu� Cw��Cy� C{� C}� C��C�� C��3C�� C���C���C�� C��3C�� C�� C���C�� C��3C�� C�� C���C�� C��3C�� C�� C���C�� C��3C��3C�� C�� C���C�� C�� C���C���C���C�� C��3C��3C��3C��3C��3C��3C��3C��3C�� C�� C�� C���C�� C��3C�� C���C���C�� C��3C��3C�� C���C�� C�� C��3C��3C��3C�� C���C���C�� C�� C�� C���C�� C�� C�� C�� C�� Cǳ3C�� C�� Cʳ3C�� C���C�� C�� Cϳ3Cг3Cѳ3Cҳ3Cӳ3C�� C���C���C���C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�3C�� C���C�� C�� C���C�� C�� C�� C�3C��3C�� C�� C�� C�� C�� C�� C��3C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� D ` D � D` D� DY�D� DffD�fD` DٚDY�DٚD` D�fDffD�fDffD�fD	` D	ٚD
Y�D
ٚDY�D� D` D� D` D� DffD� DY�DٚD` D� DffD�fDffD�fD` DٚD` D� D` D� D` D� D` D� D` D� D` D� DffD� D` D� D` D�fD` DٚD` D�fD` D� D Y�D ٚD!` D!�fD"` D"� D#` D#� D$ffD$� D%` D%�fD&` D&� D'` D'ٚD(` D(� D)` D)� D*` D*� D+` D+� D,` D,� D-` D-� D.` D.�fD/` D/� D0` D0� D1` D1ٚD2` D2� D3` D3� D4` D4� D5` D5� D6` D6� D7` D7�fD8` D8ٚD9Y�D9� D:` D:� D;Y�D;� D<` D<�fD=` D=ٚD>` D>� D?` D?� D@` D@� DA` DA� DB` DB� DC` DC� DD` DD� DE` DE�fDFffDF� DGY�DG� DH` DHٚDI` DI� DJ` DJ� DK` DK� DL` DL� DM` DM� DN` DN� DOY�DO� DPffDP� DQ` DQ� DRffDR�fDS` DS� DTY�DT� DUffDU� DV` DV� DWY�DW� DXffDX� DYffDY� DZ` DZ� D[` D[ٚD\Y�D\� D]` D]� D^` D^� D_` D_� D`` D`ٚDa` Da� DbY�Db� Dc` DcٚDd` Dd� De` De� Df` Df� Dg` Dg� Dh` DhٚDi` Di�fDjffDj� Dk` Dk� DlffDl� DmY�Dm� Dn` Dn� Do` Do� Dp` Dp� Dq` Dq� Dr` Dr� Ds` Ds� Dt` Dt� Du` Du� Dv` Dv� Dw` Dy��D��fD�#3D�p D�� D��fD�6fD�s3D���D��fD��D�|�DǓ3D��fD�3D�` D���D��D�3D�,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AĲ-Aİ!Aġ�AĬAİ!AĶFAĮAě�Aę�Aě�Aě�Aė�AčPA�~�A�p�A�bNA�O�A�-A�$�A� �A�VA���A��/AÅA�n�A�jA�ffA�dZA�^5A�VA�K�A�9XA��A���A���A��A��A���A���A©�A\A�dZA�C�A�+A�VA���A��`A�ĜA���A�VA���A��yA���A�/A��RA�VA�{A�G�A��/A��A�jA�{A���A���A�9XA�n�A���A���A�1'A�dZA�?}A�\)A��!A�E�A�r�A��A�JA�A�VA�/A��A�^5A���A��DA�JA�^5A|M�AwS�AuXAn�+AlbAh^5Ae�^Aa�A]��AY\)AP��ANAK��AI��AI|�AK�^AL��AM\)ALv�AKS�AH��AH{AF�ADZAA"�A?�wA?oA;A7p�A3�FA1p�A.�jA-C�A,�yA+�hA+�^A+�wA+�A+XA*A*A�A+ƨA+�7A+��A, �A,n�A,��A,��A,��A,v�A+��A+A*JA)�A(^5A'x�A'S�A&�`A&ĜA&jA%+A%�A$~�A#��A$-A$��A#�A#`BA"ffA!XA M�A��AC�A��A�A7LA�uAAS�A&�A��AjAbAO�A�\A$�A�;A�-AXA�A{A�PA�A�
A�#Al�A�AQ�A��A��A\)A�A&�A7LA�A~�A�AƨA�TA(�AƨA;dA�A=qAl�A5?AƨA=qAn�A�FAdZA��A-A �A�A7LA
�A	��A	�A��An�A-A�Al�A��A$�A�hA;dA��AȴA�AA�hA�#A1A�AVA�A��Az�A��A�A �A$�AS�A ��A ��A ȴA/Ax�A�At�A?}A ��@�t�@�v�@��@�o@�n�@�$�@�hs@��@�"�@�ȴ@�~�@�ff@�
=@�S�@��@�^5@�J@��^@�/@��@�w@��@�!@�^@�z�@�S�@@�J@���@�x�@�/@��/@�r�@��@�S�@�"�@�R@��@�h@�X@��`@�ƨ@���@��@�V@�D@�b@�K�@�M�@���@�^@�hs@�hs@�7L@��@�1@ߝ�@�\)@�@�{@ݩ�@ܴ9@� �@ۅ@�o@ڧ�@���@���@�9X@�|�@��y@�v�@�J@ա�@�?}@ԃ@ӥ�@���@�@���@Ѳ-@�O�@�1'@ϥ�@�t�@�"�@��@�^5@���@�`B@���@���@��@�ƨ@��@�V@��#@�@�hs@ȼj@�ƨ@�S�@��@ư!@���@őh@�?}@��@���@��/@�z�@�A�@Ý�@�5?@�@�J@�@��7@�X@�7L@���@�Z@�ƨ@��@�V@��-@��7@�Ĝ@�bN@�Z@�Q�@�9X@�b@��m@���@���@�^5@��-@���@��-@���@��@��h@�?}@�V@�z�@��@��@��@�33@���@�~�@��@��^@�&�@���@�A�@��;@��w@��P@�l�@�o@���@�~�@�n�@�V@���@���@��`@�I�@� �@�1@���@��w@��@���@��P@�dZ@�C�@�
=@�n�@��@��@���@��@�Ĝ@���@�j@�1@���@�C�@��@�ȴ@�ff@�^5@���@���@�z�@�I�@��w@�\)@�C�@�"�@�o@��R@��+@�v�@�5?@���@���@�X@��/@��u@�(�@��
@���@�K�@�@���@�~�@�{@��@��^@��@�I�@�ƨ@��P@��@�5?@��#@��^@���@��@�`B@��@���@���@�j@��@���@�|�@�l�@�S�@�33@��@���@�x�@��@���@�C�@~$�@q�#@e`B@[o@O��@F��@B��@=/@6��@/
=@(��@$�/@��@\)@11111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AĲ-Aİ!Aġ�AĬAİ!AĶFAĮAě�Aę�Aě�Aě�Aė�AčPA�~�A�p�A�bNA�O�A�-A�$�A� �A�VA���A��/AÅA�n�A�jA�ffA�dZA�^5A�VA�K�A�9XA��A���A���A��A��A���A���A©�A\A�dZA�C�A�+A�VA���A��`A�ĜA���A�VA���A��yA���A�/A��RA�VA�{A�G�A��/A��A�jA�{A���A���A�9XA�n�A���A���A�1'A�dZA�?}A�\)A��!A�E�A�r�A��A�JA�A�VA�/A��A�^5A���A��DA�JA�^5A|M�AwS�AuXAn�+AlbAh^5Ae�^Aa�A]��AY\)AP��ANAK��AI��AI|�AK�^AL��AM\)ALv�AKS�AH��AH{AF�ADZAA"�A?�wA?oA;A7p�A3�FA1p�A.�jA-C�A,�yA+�hA+�^A+�wA+�A+XA*A*A�A+ƨA+�7A+��A, �A,n�A,��A,��A,��A,v�A+��A+A*JA)�A(^5A'x�A'S�A&�`A&ĜA&jA%+A%�A$~�A#��A$-A$��A#�A#`BA"ffA!XA M�A��AC�A��A�A7LA�uAAS�A&�A��AjAbAO�A�\A$�A�;A�-AXA�A{A�PA�A�
A�#Al�A�AQ�A��A��A\)A�A&�A7LA�A~�A�AƨA�TA(�AƨA;dA�A=qAl�A5?AƨA=qAn�A�FAdZA��A-A �A�A7LA
�A	��A	�A��An�A-A�Al�A��A$�A�hA;dA��AȴA�AA�hA�#A1A�AVA�A��Az�A��A�A �A$�AS�A ��A ��A ȴA/Ax�A�At�A?}A ��@�t�@�v�@��@�o@�n�@�$�@�hs@��@�"�@�ȴ@�~�@�ff@�
=@�S�@��@�^5@�J@��^@�/@��@�w@��@�!@�^@�z�@�S�@@�J@���@�x�@�/@��/@�r�@��@�S�@�"�@�R@��@�h@�X@��`@�ƨ@���@��@�V@�D@�b@�K�@�M�@���@�^@�hs@�hs@�7L@��@�1@ߝ�@�\)@�@�{@ݩ�@ܴ9@� �@ۅ@�o@ڧ�@���@���@�9X@�|�@��y@�v�@�J@ա�@�?}@ԃ@ӥ�@���@�@���@Ѳ-@�O�@�1'@ϥ�@�t�@�"�@��@�^5@���@�`B@���@���@��@�ƨ@��@�V@��#@�@�hs@ȼj@�ƨ@�S�@��@ư!@���@őh@�?}@��@���@��/@�z�@�A�@Ý�@�5?@�@�J@�@��7@�X@�7L@���@�Z@�ƨ@��@�V@��-@��7@�Ĝ@�bN@�Z@�Q�@�9X@�b@��m@���@���@�^5@��-@���@��-@���@��@��h@�?}@�V@�z�@��@��@��@�33@���@�~�@��@��^@�&�@���@�A�@��;@��w@��P@�l�@�o@���@�~�@�n�@�V@���@���@��`@�I�@� �@�1@���@��w@��@���@��P@�dZ@�C�@�
=@�n�@��@��@���@��@�Ĝ@���@�j@�1@���@�C�@��@�ȴ@�ff@�^5@���@���@�z�@�I�@��w@�\)@�C�@�"�@�o@��R@��+@�v�@�5?@���@���@�X@��/@��u@�(�@��
@���@�K�@�@���@�~�@�{@��@��^@��@�I�@�ƨ@��P@��@�5?@��#@��^@���@��@�`B@��@���@���@�j@��@���@�|�@�l�@�S�@�33@��@���@�x�@��@���@�C�@~$�@q�#@e`B@[o@O��@F��@B��@=/@6��@/
=@(��@$�/@��@\)@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
1'B
1'B
33B
33B
33B
2-B
33B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
6FB
9XB
9XB
:^B
@�B
H�B
VB
�B
�=B
�DB
�DB
�JB
�JB
�PB
�\B
�uB
��B
��B
��B
��B
��B
�9B
�5B	7B{B �B(�B(�B/B49B:^B<jB>wBM�BP�BbNB|�B�B�B��B��BaHB?}B6FB[#B�uB�yBĜBhsBB�B
=B
��B
��BM�B�Bp�BiyBZBG�B1B�BffB
��B
}�B
"�B	�B	�B	�-B	�DB	v�B	ZB	B�B	8RB	.B	!�B	{B	1B��B�HB��BÖB�}B�wB�}B�B	VB	1'B	M�B	hsB	hsB	ffB	�bB	��B	�DB	�B	~�B	z�B	hsB	T�B	F�B	9XB	33B	6FB	C�B	K�B	ZB	r�B	~�B	�PB	�+B	��B	ȴB	��B	�TB	�B	��B
B
%B
1B
bB
hB
PB
JB
hB
oB
�B
'�B
-B
-B
)�B
&�B
+B
.B
'�B
1'B
@�B
;dB
49B
.B
(�B
(�B
&�B
!�B
�B
�B
�B
{B
oB
hB
oB
uB
{B
�B
uB
uB
{B
{B
uB
oB
hB
uB
�B
�B
�B
"�B
$�B
$�B
"�B
#�B
$�B
"�B
#�B
&�B
'�B
'�B
#�B
 �B
�B
"�B
+B
-B
'�B
"�B
"�B
�B
�B
{B
�B
!�B
�B
�B
�B
�B
�B
 �B
�B
�B
uB
VB
JB
	7B
+B
1B
B
B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
B

=B

=B
1B
+B
+B
+B
\B
�B
�B
�B
�B
oB
VB

=B
B
B
B
  B
B
  B	��B	��B	��B
B
DB
\B
bB
bB
bB
\B
VB
JB
JB
DB

=B

=B
	7B
	7B
1B
+B
1B
1B
+B
1B
1B
	7B
DB

=B

=B

=B

=B

=B
	7B

=B
	7B
1B
%B
B
B
  B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B

=B
JB
JB
JB
JB
JB
JB
JB
DB

=B
DB
DB
DB
PB
PB
PB
PB
PB
PB
PB
JB
JB
DB
DB
DB
DB
DB
JB
JB
JB
PB
VB
\B
VB
VB
PB
PB
VB
VB
PB
VB
VB
PB
PB
JB
JB
JB
JB
JB
PB
JB
DB
DB
PB
JB
JB
JB
DB
DB
DB
DB
DB

=B

=B

=B
	7B
	7B
1B
	7B

=B

=B
	7B
	7B
	7B
1B
1B
1B
+B
%B
B
B
B
%B
%B
%B
%B
%B
%B
%B
%B
+B
+B
+B
+B
1B
	7B
	7B
	7B
	7B
	7B
DB
bB
�B
!�B
(�B
/B
6FB
<jB
B�B
J�B
Q�B
VB
ZB
^5B
cTB
jB
m�B
t�B
x�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
1'B
2-B
33B
33B
33B
33B
49B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
6FB
7LB
:^B
9XB
;dB
A�B
J�B
YB
�B
�=B
�DB
�DB
�JB
�PB
�VB
�bB
�{B
��B
��B
��B
��B
��B
�?B
�HB
=B�B!�B)�B)�B0!B5?B<jB>wBC�BQ�BZBl�B�B�VB��B�BǮB� BJ�B6FBW
B��B
=B�B~�B[#B{B
��B
�`B'�B�B|�B|�Bl�Bk�B(�B\B��BA�B
�wB
L�B
VB	��B	��B	��B	�{B	y�B	W
B	]/B	@�B	9XB	(�B	!�B	hB	  B��B��B��BƨB�jB��B	1B	2-B	VB	s�B	w�B	m�B	��B	�B	��B	�PB	�PB	��B	�B	q�B	YB	J�B	<jB	;dB	J�B	K�B	[#B	t�B	�B	�uB	�B	�bB	ɺB	��B	�HB	�B	��B
%B
+B
DB
�B
�B
{B
oB
�B
�B
�B
+B
0!B
2-B
1'B
(�B
/B
1'B
%�B
0!B
G�B
A�B
<jB
7LB
0!B
-B
,B
&�B
&�B
 �B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
%�B
(�B
)�B
%�B
%�B
'�B
$�B
#�B
'�B
)�B
,B
&�B
!�B
�B
"�B
/B
2-B
-B
&�B
)�B
%�B
�B
oB
�B
&�B
#�B
#�B
�B
�B
�B
'�B
!�B
�B
�B
uB
\B
JB

=B
PB

=B
+B
B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
B
JB
\B

=B
1B
+B
B
VB
�B
�B
�B
�B
�B
uB
bB

=B
B
B
B
B
B	��B	��B	��B
B
DB
\B
uB
bB
bB
\B
bB
JB
JB
DB

=B

=B
PB
	7B
1B
+B
1B
	7B
	7B

=B

=B
DB
DB
JB

=B

=B
JB
PB
	7B

=B
	7B
1B
1B
B
%B
B
  B	��B	��B
B
B
B	��B
  B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B
  B	��B	��B	��B	��B	��B
B	��B
B
B
B
B	��B
  B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
%B
%B
1B

=B
JB
PB
JB
PB
VB
JB
VB
PB
JB
JB
JB
JB
VB
VB
VB
VB
VB
VB
PB
PB
JB
VB
PB
JB
DB
DB
JB
JB
JB
VB
VB
bB
\B
bB
PB
VB
VB
\B
PB
\B
\B
VB
VB
JB
JB
PB
PB
PB
\B
VB
PB
JB
\B
JB
PB
PB
DB
JB
JB
DB
JB
DB
DB
DB

=B

=B
	7B

=B
DB

=B

=B
	7B
	7B
	7B
	7B
	7B
+B
1B
B
%B
+B
1B
+B
+B
+B
%B
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

=B

=B
	7B
JB
bB
�B
!�B
(�B
/B
6FB
<jB
B�B
J�B
Q�B
VB
ZB
_;B
dZB
jB
m�B
t�B
x�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<�h=ix�=,1<�h<49X<#�
<#�
<#�
=o=�P<�9X<ě�<#�
<#�
<�1=�P<#�
<49X<���<�C�=C�<��=]/=�%=�\)=�%='�=+<�h<�/<u<�h<��<��
=t�<�t�<�j<��
<���<�/<�=8Q�<�o<e`B<#�
<#�
<D��<#�
<#�
<#�
<49X<u<#�
<49X<��
<��
<D��<e`B<�<�`B<�/<�t�<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.5 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250152012011312501520120113125015  AO  ARGQ                                                                        20111205112721  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112721  QCF$                G�O�G�O�G�O�800             UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125015  IP                  G�O�G�O�G�O�                