CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:37:15Z creation;2022-06-04T17:37:16Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604173715  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               QA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�o�;�0*1   @�o����u@.H1&�x��c���l�D1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @@  @�  @�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB癚B���B�  B�  B�  B�  C   C  C  C�fC  C	�fC  C33C  C�C�3C�fC  C  C  C  C   C"  C$  C&  C(  C*�C,  C-�fC0  C1�fC4  C6  C8  C:  C<  C>  C?�fCB  CD  CE�fCH  CI�fCL  CN  CP  CR  CT  CU�fCX  CZ  C\33C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @0  @p  @�  @�  A  A<  A\  AzffA�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B  B'  B/  B7  B?  BG  BO  BW  B_  Bg  Bo  Bw  BffB�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BÀ Bǀ Bˀ Bπ BӀ B׀ Bۀ B߀ B��fB��B�L�B� B� B�� B�� B�� C� C� C�fC� C	�fC� C�3C� CٚCs3C�fC� C� C� C� C� C!� C#� C%� C'� C)ٚC+� C-�fC/� C1�fC3� C5� C7� C9� C;� C=� C?�fCA� CC� CE�fCG� CI�fCK� CM� CO� CQ� CS� CU�fCW� CY� C[�3C]� C_� Ca� Cc�fCe� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3C��3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C��3C�� C�� C�� C�� C�� C�� C�� C�� D p D � Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D	p D	� D
p D
� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D�fD p D � D!p D!� D"p D"� D#p D#� D$p D$� D%p D%� D&p D&� D'p D'� D(p D(� D)p D)� D*p D*� D+p D+� D,p D,� D-p D-� D.p D.� D/p D/� D0p D0� D1p D1� D2p D2� D3p D3� D4p D4� D5p D5� D6p D6� D7p D7� D8p D8� D9p D9� D:p D:� D;p D;� D<p D<� D=p D=� D>p D>� D?p D?� D@p D@� DAp DA� DBp DB� DCp DC� DDp DD� DEp DE� DFp DF� DGp DG� DHp DH� DIp DI� DJp DJ� DKp DK� DLp DL� DMp DM� DNp DN� DOp DO� DPp DP� DQp DQ� DRvfDR� DSp DS� DTp DT� DUp DU� DVp DV� DWp DW� DXp DX� DYp DY� DZp DZ� D[p D[� D\p D\� D]p D]� D^p D^� D_p D_� D`p D`� Dap Da� Dbp Db� Dcp Dc� Ddp Dd� Dep De� Dfp Df� Dgp Dg� Dhp Dh� Dip Di� Djp Dj� Dkp Dk� Dlp Dl� Dmp Dm� Dnp Dn� Dop Do� Dpp Dp� Dqp Dq� Drp Dr� Dsp Ds� Dtp Dt� Dup Du� Dvp Dv� Dwp Dw� Dxp Dx� Dyp Dy� Dzp Dz� D{p D{� D|p D|� D}p D}� D~p D~� Dp D� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D¸ D�� D�8 D�x Dø D�� D�8 D�x Dĸ D�� D�8 D�x DŸ D�� D�8 D�x DƸ D�� D�8 D�x DǸ D�� D�8 D�x Dȸ D�� D�8 D�x Dɸ D�� D�8 D�x Dʸ D�� D�8 D�x D˸ D�� D�8 D�x D̸ D�� D�8 D�x D͸ D�� D�8 D�x Dθ D�� D�8 D�x Dϸ D�� D�8 D�x Dи D�� D�8 D�x DѸ D�� D�8 D�x DҸ D�� D�8 D�x DӸ D�� D�8 D�x DԸ D�� D�8 D�x Dո D�� D�8 D�x Dָ D�� D�8 D�x D׸ D�� D�8 D�x Dظ D�� D�8 D�x Dٸ D�� D�8 D�x Dڸ D�� D�8 D�x D۸ D�� D�8 D�x Dܸ D�� D�8 D�x Dݸ D�� D�8 D�x D޸ D�� D�8 D�x D߸ D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�4�D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D�� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aˆ%AˋxA˛�A˞OA˛�A˟!Aˡ-A˜CA˛=AˣnAˤtA˧A˘_Aˊ	A�t�A�l�A�o5A�V�A��Aʠ\A�H�A���A�m�A�EA� 'A���AȒ�A�^�A��A��A���Aǭ�AǝA�u�A�@OA�{A��A���A��A��HAơ�A��ZAş�A�QNA��JAĠ�A�n�AĤ�A���A�A�J�A¬A�{JA�b�A�.A��.A�IA�eA�#:A���A�$�A���A��7A�уA��~A��oA���A��A�8�A���A���A�A�ΥA�lWA��A��A�>A�VmA��A���A�cA��A��A���A�e�A�y>A��	A��TA�b�A�qAA��2A��oA�{A��A��A��aA�@�A�u�A��xA�d�A��gA�u�A�5A�JA��fA��6A��-A��A��sA�7�A��5A�eA��GA~��Az+kAx��Av�hAt�ApOAl��Ai0�Ae��AeJ�AdA`�A^3�AY�QAV͟AT�1AQ�:AN��AJ�.AH-AE�AAC�`AA+A=OA;��A:�1A7��A3-�A2?�A0��A0%FA/��A.�A,�4A+֡A+2�A)��A(��A)@A(��A(��A(��A)TaA*u�A,%A-��A-�DA.6�A,�hA*��A)w2A)~(A)ԕA+L0A+X�A+�A*ƨA*�AA*-A(33A'�WA(�A'm]A'K�A'�A'm]A&�A&g�A%�QA%_pA%A$M�A#.�A"�PA"�A!~�A ��A�uA�A�6A�]A�A��A4A��AL0A�]A{JAMA1'AMA�~AA�A~A��A�AݘA<�A1�A�A�A��A��A�oAl�A�|A1�AA�pA�*A|�A1'A��A�)AjA��A��A}�A�A��AK^A�AB[A�]A��AL0A�A
�aA	��A	��A	V�A�FAA��A�A�'A��Ak�A�A��A(Ao AxlAOA�)AA($A{A��AԕA��AA��A9�A �5@���@���@�Ɇ@�/�@��@�e@�F�@�B[@��c@�j@�)�@���@��@��@�/�@@@�M@�:�@�_�@��@���@��j@��@��;@�ں@�S�@�Vm@ꀝ@��@� \@��v@��U@跀@�6@��@�s�@�Ft@�� @�@�E�@�b@��@�Xy@��@�O@�H@�ƨ@��5@���@���@�~(@�!@�<6@ܼj@��B@��@���@�8�@���@�}V@��#@�4@��,@�bN@��z@��@���@�qv@���@�I�@�$�@�u@�0U@���@���@Ջ�@��	@��'@ԡb@ԕ@�Z@�@�ԕ@��@Ҽj@҄�@��@�dZ@�?}@��	@���@ϼ@�ϫ@ϐ�@�@��@�b�@̾@��K@�֡@�h�@�m]@��p@�`�@��;@��@�[W@�~@�|@�&@Ƶ@�a|@��r@�� @��a@ŝ�@Ń{@�T�@��@�w�@�7�@��@�qv@�_p@��@�s�@��@��@���@�Ĝ@��@�B[@���@�=@��?@�(�@��H@��@���@���@�`�@�5?@��@��@�@�j@��^@�@�m�@���@�.I@�ѷ@���@��@���@�/@���@�xl@�g8@�YK@�6�@�b@�@@���@�a|@�=q@��3@�}�@�+@���@�Z@�(�@� �@���@�[W@���@���@�Ta@�@��@��@� \@���@�֡@�͟@���@�>B@��w@�`B@��@��@�[�@�)�@���@�_p@���@�-@�l"@�kQ@���@���@�E9@��@��p@��Y@�S�@��@��@�\)@�;@��?@��.@�H�@��@��9@���@���@��@�B�@��D@��@��q@�F@��@���@�h�@���@�~�@�E9@��@��<@���@�1@���@�Q�@�
=@��@�s�@�!�@�xl@��N@��M@�U�@�+@�%@��]@��F@��@��&@���@�H�@�ߤ@�V�@��@�e�@�B�@���@���@�q@�N�@�J�@��@�E9@��v@���@���@���@���@��@�@�@��@���@��*@���@�	l@��@�D�@�#:@�7@�4@��@���@���@�e,@�@�@���@���@���@�ff@��@�x�@�RT@��@���@�֡@���@��\@�z@�=q@�� @��V@�f�@���@�_�@��@��Q@��@�a@�5�@��@��@���@�͟@��<@���@���@�4n@��@�j�@�Dg@�C@��@�Ɇ@��_@��@���@�o�@�rG@�b�@�5�@���@���@�h
@�J�@�~@���@�c�@��@�ߤ@��R@��@�xl@�oi@�W�@�0U@��@�  @��9@���@�Y�@�=@��s@��@�n�@�2�@��@iD@~�}@~=q@}�@}o @}:�@|��@|Xy@{�&@z� @y��@y=�@xw�@w�V@w8@v��@u@@sj�@rl�@q��@q8�@pی@ptT@p'R@oo�@o@oC@n�6@m�-@mu�@m7L@l��@lK^@k��@k��@kO@j�2@j��@j?@i�>@i=�@i&�@i�@h�[@h��@h�Y@h*�@f҉@f�@e��@d�I@dK^@c�Q@b҉@bn�@bTa@b@a��@a�@a��@ax�@a\�@a�@`K^@_�@^�"@^� @]�@]V@\K^@[��@[�V@[E9@Z��@Z�@Zz@Ze@Y��@X:�@WdZ@W
=@V��@V�1@Va|@V�@U��@U*0@T��@T�P@T��@T��@T�@S�6@S@R��@RQ@R{@Q�@Q��@P�	@P:�@O�]@O�{@N{�@N#:@Mԕ@M*0@L$@K��@Kt�@K�@Jߤ@J�X@J��@J�b@J=q@I��@I�d@I�@I��@If�@I�@H�[@H��@H,=@G��@G9�@G!-@F�L@F�@E�@E�@D��@C�A@CRT@C�@B�"@Bں@B��@BC�@B$�@A��@A�X@A�@AX@A�@@��@?�]@?��@?C�@>�"@>��@>ff@>�@=ϫ@=zx@=�@<Xy@<,=@<�@;��@;g�@;A�@:��@:҉@:��@:kQ@9�.@9��@9k�@9J�@9N<@9J�@9A @9�@8�j@8�@7�@7��@733@7(@6�<@6u%@6=q@6($@5rG@4��@4�@4~(@47@3C@2�'@2��@2}V@2$�@1��@1�@0��@0��@0bN@0!@/�@/��@/dZ@/S�@/P�@/J#@/C�@/@.�B@.�F@.#:@-�9@-��@-k�@,�|@,�e@,M@+��@+8@*��@*�1@*��@*YK@*�@)�@)o @)^�@)8�@)�@(�@(��@(]d@(H@(	�@'ݘ@']�@&�2@&�@&��@&�+@&a|@&�@%�@%��@%T�@$�f@$��@$�@$m�@$`�@$-�@#�A@#��@#9�@"�8@"�@"�\@"ff@"C�@"�@!ԕ@!��@!�"@!�~@!<6@ ��@ ��@ :�@ 7@��@��@{J@RT@;d@�@�\@�+@d�@+k@�@��@��@�C@�S@Q�@�@�O@�@tT@`�@A�@�@�@@��@;d@1�@Y@�M@�s@�@�}@�h@�+@	@��@rG@Dg@��@�u@�.@�@�@�@j@@�@�W@�@@ �@�W@��@��@�@J�@�5@��@h�@Z@�@�A@�6@�f@{J@]�@Y@(@��@҉@��@�@q�@V@�@ԕ@�C@��@��@rG@\�@0�@%F@;@�)@��@l"@�]@�&@�;@�[@��@]�@$t@�@�@͟@��@\�@�@��@�'@a�@Q�@L�@2a@q@�@��@c�@I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aˆ%AˋxA˛�A˞OA˛�A˟!Aˡ-A˜CA˛=AˣnAˤtA˧A˘_Aˊ	A�t�A�l�A�o5A�V�A��Aʠ\A�H�A���A�m�A�EA� 'A���AȒ�A�^�A��A��A���Aǭ�AǝA�u�A�@OA�{A��A���A��A��HAơ�A��ZAş�A�QNA��JAĠ�A�n�AĤ�A���A�A�J�A¬A�{JA�b�A�.A��.A�IA�eA�#:A���A�$�A���A��7A�уA��~A��oA���A��A�8�A���A���A�A�ΥA�lWA��A��A�>A�VmA��A���A�cA��A��A���A�e�A�y>A��	A��TA�b�A�qAA��2A��oA�{A��A��A��aA�@�A�u�A��xA�d�A��gA�u�A�5A�JA��fA��6A��-A��A��sA�7�A��5A�eA��GA~��Az+kAx��Av�hAt�ApOAl��Ai0�Ae��AeJ�AdA`�A^3�AY�QAV͟AT�1AQ�:AN��AJ�.AH-AE�AAC�`AA+A=OA;��A:�1A7��A3-�A2?�A0��A0%FA/��A.�A,�4A+֡A+2�A)��A(��A)@A(��A(��A(��A)TaA*u�A,%A-��A-�DA.6�A,�hA*��A)w2A)~(A)ԕA+L0A+X�A+�A*ƨA*�AA*-A(33A'�WA(�A'm]A'K�A'�A'm]A&�A&g�A%�QA%_pA%A$M�A#.�A"�PA"�A!~�A ��A�uA�A�6A�]A�A��A4A��AL0A�]A{JAMA1'AMA�~AA�A~A��A�AݘA<�A1�A�A�A��A��A�oAl�A�|A1�AA�pA�*A|�A1'A��A�)AjA��A��A}�A�A��AK^A�AB[A�]A��AL0A�A
�aA	��A	��A	V�A�FAA��A�A�'A��Ak�A�A��A(Ao AxlAOA�)AA($A{A��AԕA��AA��A9�A �5@���@���@�Ɇ@�/�@��@�e@�F�@�B[@��c@�j@�)�@���@��@��@�/�@@@�M@�:�@�_�@��@���@��j@��@��;@�ں@�S�@�Vm@ꀝ@��@� \@��v@��U@跀@�6@��@�s�@�Ft@�� @�@�E�@�b@��@�Xy@��@�O@�H@�ƨ@��5@���@���@�~(@�!@�<6@ܼj@��B@��@���@�8�@���@�}V@��#@�4@��,@�bN@��z@��@���@�qv@���@�I�@�$�@�u@�0U@���@���@Ջ�@��	@��'@ԡb@ԕ@�Z@�@�ԕ@��@Ҽj@҄�@��@�dZ@�?}@��	@���@ϼ@�ϫ@ϐ�@�@��@�b�@̾@��K@�֡@�h�@�m]@��p@�`�@��;@��@�[W@�~@�|@�&@Ƶ@�a|@��r@�� @��a@ŝ�@Ń{@�T�@��@�w�@�7�@��@�qv@�_p@��@�s�@��@��@���@�Ĝ@��@�B[@���@�=@��?@�(�@��H@��@���@���@�`�@�5?@��@��@�@�j@��^@�@�m�@���@�.I@�ѷ@���@��@���@�/@���@�xl@�g8@�YK@�6�@�b@�@@���@�a|@�=q@��3@�}�@�+@���@�Z@�(�@� �@���@�[W@���@���@�Ta@�@��@��@� \@���@�֡@�͟@���@�>B@��w@�`B@��@��@�[�@�)�@���@�_p@���@�-@�l"@�kQ@���@���@�E9@��@��p@��Y@�S�@��@��@�\)@�;@��?@��.@�H�@��@��9@���@���@��@�B�@��D@��@��q@�F@��@���@�h�@���@�~�@�E9@��@��<@���@�1@���@�Q�@�
=@��@�s�@�!�@�xl@��N@��M@�U�@�+@�%@��]@��F@��@��&@���@�H�@�ߤ@�V�@��@�e�@�B�@���@���@�q@�N�@�J�@��@�E9@��v@���@���@���@���@��@�@�@��@���@��*@���@�	l@��@�D�@�#:@�7@�4@��@���@���@�e,@�@�@���@���@���@�ff@��@�x�@�RT@��@���@�֡@���@��\@�z@�=q@�� @��V@�f�@���@�_�@��@��Q@��@�a@�5�@��@��@���@�͟@��<@���@���@�4n@��@�j�@�Dg@�C@��@�Ɇ@��_@��@���@�o�@�rG@�b�@�5�@���@���@�h
@�J�@�~@���@�c�@��@�ߤ@��R@��@�xl@�oi@�W�@�0U@��@�  @��9@���@�Y�@�=@��s@��@�n�@�2�@��@iD@~�}@~=q@}�@}o @}:�@|��@|Xy@{�&@z� @y��@y=�@xw�@w�V@w8@v��@u@@sj�@rl�@q��@q8�@pی@ptT@p'R@oo�@o@oC@n�6@m�-@mu�@m7L@l��@lK^@k��@k��@kO@j�2@j��@j?@i�>@i=�@i&�@i�@h�[@h��@h�Y@h*�@f҉@f�@e��@d�I@dK^@c�Q@b҉@bn�@bTa@b@a��@a�@a��@ax�@a\�@a�@`K^@_�@^�"@^� @]�@]V@\K^@[��@[�V@[E9@Z��@Z�@Zz@Ze@Y��@X:�@WdZ@W
=@V��@V�1@Va|@V�@U��@U*0@T��@T�P@T��@T��@T�@S�6@S@R��@RQ@R{@Q�@Q��@P�	@P:�@O�]@O�{@N{�@N#:@Mԕ@M*0@L$@K��@Kt�@K�@Jߤ@J�X@J��@J�b@J=q@I��@I�d@I�@I��@If�@I�@H�[@H��@H,=@G��@G9�@G!-@F�L@F�@E�@E�@D��@C�A@CRT@C�@B�"@Bں@B��@BC�@B$�@A��@A�X@A�@AX@A�@@��@?�]@?��@?C�@>�"@>��@>ff@>�@=ϫ@=zx@=�@<Xy@<,=@<�@;��@;g�@;A�@:��@:҉@:��@:kQ@9�.@9��@9k�@9J�@9N<@9J�@9A @9�@8�j@8�@7�@7��@733@7(@6�<@6u%@6=q@6($@5rG@4��@4�@4~(@47@3C@2�'@2��@2}V@2$�@1��@1�@0��@0��@0bN@0!@/�@/��@/dZ@/S�@/P�@/J#@/C�@/@.�B@.�F@.#:@-�9@-��@-k�@,�|@,�e@,M@+��@+8@*��@*�1@*��@*YK@*�@)�@)o @)^�@)8�@)�@(�@(��@(]d@(H@(	�@'ݘ@']�@&�2@&�@&��@&�+@&a|@&�@%�@%��@%T�@$�f@$��@$�@$m�@$`�@$-�@#�A@#��@#9�@"�8@"�@"�\@"ff@"C�@"�@!ԕ@!��@!�"@!�~@!<6@ ��@ ��@ :�@ 7@��@��@{J@RT@;d@�@�\@�+@d�@+k@�@��@��@�C@�S@Q�@�@�O@�@tT@`�@A�@�@�@@��@;d@1�@Y@�M@�s@�@�}@�h@�+@	@��@rG@Dg@��@�u@�.@�@�@�@j@@�@�W@�@@ �@�W@��@��@�@J�@�5@��@h�@Z@�@�A@�6@�f@{J@]�@Y@(@��@҉@��@�@q�@V@�@ԕ@�C@��@��@rG@\�@0�@%F@;@�)@��@l"@�]@�&@�;@�[@��@]�@$t@�@�@͟@��@\�@�@��@�'@a�@Q�@L�@2a@q@�@��@c�@I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�FB	��B	�oB	�:B	�TB	� B	� B	��B	�&B	��B	��B	�&B	�SB	�B	�B	�OB	��B	�bB	��B	�0B	�?B	ӏB	�bB	�B	��B	�B	�B	�<B
�B
zB
B
vB
�B
�B
�B
�B
TB
4B
�B
JB
�B	�}B	�ZB	�	B	� B	��B	��B	�B
4B
 \B
DB
 iB
  B
JB
�B
�B
�B
!�B
4�B
=�B
EmB
{0B
�#B
B
�BB@�BA�BIlB\]Bw�B��B�pB�pB��B��B��B�SB�QB��BݲB�B�KB��B�GB�AB��B�XBMB��B�(B��B��B�B�B��B��B�mB��Bq[BNBEmB.�B�B
�fB
��B
�hB
��B
�B
y�B
m�B
F%B
'�B
B	�;B	�LB	՛B	�4B	��B	�.B	}�B	i�B	abB	Z�B	L�B	A�B	3B	'8B	B	B	�B	�B��B�B�B�zB�3B�B�vB��B�jB��B�.B	 �B	'B	�B		�B	
	B	�B	
#B	�B	7B	DgB	I�B	V9B	rGB	�>B	��B
	RB
�B
~B
yB
?B	��B
 �B
	�B
$�B
3�B
0oB
-�B
.�B
/OB
 \B
VB
6�B
72B
8�B
D�B
L�B
KB
IB
HB
J	B
MjB
LB
D�B
H�B
O�B
P�B
MB
IRB
L~B
QNB
MB
G�B
E�B
D�B
F�B
ESB
H�B
IRB
H1B
MB
P�B
K�B
L0B
O�B
NpB
LdB
L~B
K�B
D�B
C�B
IB
CaB
E�B
E�B
E�B
D�B
C-B
C�B
D�B
DgB
D�B
DMB
DMB
C�B
D3B
CB
B[B
C-B
C�B
B�B
A�B
AUB
=�B
<�B
;JB
9�B
4�B
+�B
�B
$&B
&2B
"�B
�B
�B
�B
%`B
(XB
)B
(�B
%�B
%�B
"�B
#�B
#�B
 \B
�B
7B
�B
B
�B
_B
�B
�B
 B
vB

=B
{B
 4B	��B	�XB	�ZB	�B	��B	��B	�B	�sB	�B	�@B	��B	�dB	�~B	�qB	�qB	��B	��B	�6B	�6B	�6B	�B	�B	�6B	��B	�B	��B	�B	�sB	�>B	��B	�B	�RB	�RB	��B	�RB	�DB	�0B	�*B	�}B	�UB	��B	��B	�}B	�B	�-B	�oB	�iB	�=B	��B	�]B	��B	��B	��B	�aB	��B	�B	��B	�<B	�PB	�B	�B	�B	��B	��B	��B	�B	��B	��B	�zB	�fB	��B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�HB	��B	�(B	��B	��B	�cB	�]B	�(B	��B
  B	�}B	�"B	��B	��B
 4B
  B	��B	��B	��B	�PB	��B	��B	�0B	�B	�XB	��B	�B	��B	�jB	��B	�<B	�B	��B	�wB	��B	��B	�HB	�B	��B	��B	��B	�.B	��B	�B	�B	�B	��B	��B	�]B	��B	�cB	�cB	�cB	��B	��B	��B	�.B
  B
 �B
�B
�B
oB
�B
B
B
�B
�B
{B
GB
GB
�B
�B
B
GB
aB
�B
�B
B
�B
MB
�B
YB
�B
�B
�B
zB
�B
�B
	lB
	�B
	�B

XB

rB

�B

�B

�B

�B
)B
)B
^B
xB
xB
DB
0B
�B
~B
�B
�B
<B
B
�B

rB
	�B
B
JB
PB
PB
�B
�B
<B
�B
�B
jB
�B
�B
JB
B
�B
�B
�B
�B
NB
�B
:B
B
B
oB
 B
�B
�B
�B
 B
TB
:B
�B
TB
�B
&B
uB
@B
�B
�B
9B
B
�B
SB

B
?B
YB

B

B
�B
�B
?B
�B
�B
�B
�B
�B
1B
�B
�B
B
7B
kB
�B
7B
kB
	B
qB
�B
�B
�B
�B
xB
�B
�B
B
dB
/B
/B
�B
�B
�B
�B
�B
�B
!B
�B
�B
 \B
 \B
 vB
 BB
 �B
 �B
!|B
!�B
!�B
"�B
"�B
"�B
# B
#nB
#nB
#�B
$�B
$�B
$�B
%FB
%�B
%zB
%�B
%�B
&fB
&�B
&�B
&�B
&�B
'B
&�B
&�B
'B
'�B
(XB
(�B
)B
(�B
)B
(�B
)�B
*B
*�B
*�B
*�B
*B
+6B
+QB
+�B
+QB
+�B
+�B
,�B
,�B
-�B
-�B
./B
-�B
./B
-�B
-�B
.�B
.cB
.�B
.�B
/5B
/B
/B
/�B
/�B
/�B
/�B
0!B
0�B
1vB
1�B
2GB
2|B
2aB
2aB
2-B
1�B
2B
1�B
1AB
0�B
0�B
0UB
0�B
0UB
/iB
/5B
/OB
0�B
1[B
0�B
0�B
0UB
/�B
0;B
0�B
0B
/�B
/�B
/�B
0B
0UB
0UB
0�B
0�B
1�B
2B
2-B
49B
4B
4�B
5B
5tB
5ZB
5�B
6B
5tB
6B
6zB
6FB
6�B
7�B
8B
8B
8RB
8lB
8�B
8�B
9	B
:*B
;�B
;�B
;�B
<6B
<B
<PB
<�B
=qB
=�B
=qB
>wB
>�B
>BB
>]B
?B
?.B
?�B
?�B
?cB
@4B
@�B
A;B
BB
B�B
CaB
C�B
C{B
CaB
C{B
C�B
CGB
C�B
C�B
C�B
C�B
C�B
C�B
C�B
CaB
C-B
CB
C�B
C{B
C�B
C�B
D�B
EB
D�B
ESB
FB
E�B
E�B
E�B
E�B
FtB
FtB
FYB
F�B
G�B
GEB
G_B
G+B
GzB
G�B
G�B
G�B
H�B
H�B
IB
H�B
J=B
J�B
L0B
L~B
M6B
M�B
N<B
N�B
N�B
N�B
O�B
O�B
O�B
P.B
P}B
P�B
QhB
Q4B
Q�B
Q4B
Q�B
Q�B
Q�B
RTB
R:B
SuB
S�B
S�B
TB
T�B
T�B
T�B
T�B
UgB
U�B
VmB
W�B
Y1B
X�B
XEB
XyB
X_B
X�B
X+B
YB
X�B
Y�B
Z�B
[	B
[#B
[	B
[qB
\�B
^�B
^�B
^jB
^B
^�B
_B
_B
_;B
_�B
_pB
`BB
`vB
`�B
`vB
aB
`�B
aHB
a�B
a�B
a�B
a�B
a�B
a|B
b4B
b4B
b�B
b�B
c�B
b�B
c B
cTB
c�B
c�B
d�B
d�B
e`B
e�B
ezB
e�B
e�B
e�B
f�B
e�B
f�B
fB
ffB
f�B
g�B
f�B
g�B
g�B
h�B
h�B
h�B
hsB
i*B
h�B
iDB
i_B
i�B
i�B
i�B
i�B
i�B
i�B
jKB
jeB
j0B
j�B
kQB
kB
k�B
k�B
k�B
l"B
k�B
lWB
l=B
l"B
k�B
l�B
lqB
l�B
l�B
mB
m]B
m�B
m�B
m�B
m�B
nIB
n�B
ncB
n}B
n�B
o�B
o�B
o5B
o B
n�B
oiB
oiB
oB
o�B
o�B
o�B
o�B
o�B
o�B
p!B
p�B
p;B
p!B
p�B
p�B
p�B
qAB
p�B
qB
q[B
qvB
q�B
rGB
r�B
r�B
s�B
shB
sB
r�B
s�B
sMB
s�B
s�B
tTB
t�B
vzB
vzB
vB
v+B
u�B
u�B
u�B
u�B
u�B
uZB
u�B
v`B
vB
w2B
v�B
wLB
w�B
w2B
w�B
w�B
w�B
w�B
xRB
x�B
x�B
x�B
y>B
z*B
yrB
y�B
y�B
z�B
z�B
{dB
{JB
{B
|jB
|�B
}B
|�B
}�B
}�B
}�B
~wB
~�B
~]B
~�B
~�B
.B
cB
}B
�B
�B
�B
�B
�4B
�OB
�4B
�iB
�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�FB	��B	�oB	�:B	�TB	� B	� B	��B	�&B	��B	��B	�&B	�SB	�B	�B	�OB	��B	�bB	��B	�0B	�?B	ӏB	�bB	�B	��B	�B	�B	�<B
�B
zB
B
vB
�B
�B
�B
�B
TB
4B
�B
JB
�B	�}B	�ZB	�	B	� B	��B	��B	�B
4B
 \B
DB
 iB
  B
JB
�B
�B
�B
!�B
4�B
=�B
EmB
{0B
�#B
B
�BB@�BA�BIlB\]Bw�B��B�pB�pB��B��B��B�SB�QB��BݲB�B�KB��B�GB�AB��B�XBMB��B�(B��B��B�B�B��B��B�mB��Bq[BNBEmB.�B�B
�fB
��B
�hB
��B
�B
y�B
m�B
F%B
'�B
B	�;B	�LB	՛B	�4B	��B	�.B	}�B	i�B	abB	Z�B	L�B	A�B	3B	'8B	B	B	�B	�B��B�B�B�zB�3B�B�vB��B�jB��B�.B	 �B	'B	�B		�B	
	B	�B	
#B	�B	7B	DgB	I�B	V9B	rGB	�>B	��B
	RB
�B
~B
yB
?B	��B
 �B
	�B
$�B
3�B
0oB
-�B
.�B
/OB
 \B
VB
6�B
72B
8�B
D�B
L�B
KB
IB
HB
J	B
MjB
LB
D�B
H�B
O�B
P�B
MB
IRB
L~B
QNB
MB
G�B
E�B
D�B
F�B
ESB
H�B
IRB
H1B
MB
P�B
K�B
L0B
O�B
NpB
LdB
L~B
K�B
D�B
C�B
IB
CaB
E�B
E�B
E�B
D�B
C-B
C�B
D�B
DgB
D�B
DMB
DMB
C�B
D3B
CB
B[B
C-B
C�B
B�B
A�B
AUB
=�B
<�B
;JB
9�B
4�B
+�B
�B
$&B
&2B
"�B
�B
�B
�B
%`B
(XB
)B
(�B
%�B
%�B
"�B
#�B
#�B
 \B
�B
7B
�B
B
�B
_B
�B
�B
 B
vB

=B
{B
 4B	��B	�XB	�ZB	�B	��B	��B	�B	�sB	�B	�@B	��B	�dB	�~B	�qB	�qB	��B	��B	�6B	�6B	�6B	�B	�B	�6B	��B	�B	��B	�B	�sB	�>B	��B	�B	�RB	�RB	��B	�RB	�DB	�0B	�*B	�}B	�UB	��B	��B	�}B	�B	�-B	�oB	�iB	�=B	��B	�]B	��B	��B	��B	�aB	��B	�B	��B	�<B	�PB	�B	�B	�B	��B	��B	��B	�B	��B	��B	�zB	�fB	��B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�HB	��B	�(B	��B	��B	�cB	�]B	�(B	��B
  B	�}B	�"B	��B	��B
 4B
  B	��B	��B	��B	�PB	��B	��B	�0B	�B	�XB	��B	�B	��B	�jB	��B	�<B	�B	��B	�wB	��B	��B	�HB	�B	��B	��B	��B	�.B	��B	�B	�B	�B	��B	��B	�]B	��B	�cB	�cB	�cB	��B	��B	��B	�.B
  B
 �B
�B
�B
oB
�B
B
B
�B
�B
{B
GB
GB
�B
�B
B
GB
aB
�B
�B
B
�B
MB
�B
YB
�B
�B
�B
zB
�B
�B
	lB
	�B
	�B

XB

rB

�B

�B

�B

�B
)B
)B
^B
xB
xB
DB
0B
�B
~B
�B
�B
<B
B
�B

rB
	�B
B
JB
PB
PB
�B
�B
<B
�B
�B
jB
�B
�B
JB
B
�B
�B
�B
�B
NB
�B
:B
B
B
oB
 B
�B
�B
�B
 B
TB
:B
�B
TB
�B
&B
uB
@B
�B
�B
9B
B
�B
SB

B
?B
YB

B

B
�B
�B
?B
�B
�B
�B
�B
�B
1B
�B
�B
B
7B
kB
�B
7B
kB
	B
qB
�B
�B
�B
�B
xB
�B
�B
B
dB
/B
/B
�B
�B
�B
�B
�B
�B
!B
�B
�B
 \B
 \B
 vB
 BB
 �B
 �B
!|B
!�B
!�B
"�B
"�B
"�B
# B
#nB
#nB
#�B
$�B
$�B
$�B
%FB
%�B
%zB
%�B
%�B
&fB
&�B
&�B
&�B
&�B
'B
&�B
&�B
'B
'�B
(XB
(�B
)B
(�B
)B
(�B
)�B
*B
*�B
*�B
*�B
*B
+6B
+QB
+�B
+QB
+�B
+�B
,�B
,�B
-�B
-�B
./B
-�B
./B
-�B
-�B
.�B
.cB
.�B
.�B
/5B
/B
/B
/�B
/�B
/�B
/�B
0!B
0�B
1vB
1�B
2GB
2|B
2aB
2aB
2-B
1�B
2B
1�B
1AB
0�B
0�B
0UB
0�B
0UB
/iB
/5B
/OB
0�B
1[B
0�B
0�B
0UB
/�B
0;B
0�B
0B
/�B
/�B
/�B
0B
0UB
0UB
0�B
0�B
1�B
2B
2-B
49B
4B
4�B
5B
5tB
5ZB
5�B
6B
5tB
6B
6zB
6FB
6�B
7�B
8B
8B
8RB
8lB
8�B
8�B
9	B
:*B
;�B
;�B
;�B
<6B
<B
<PB
<�B
=qB
=�B
=qB
>wB
>�B
>BB
>]B
?B
?.B
?�B
?�B
?cB
@4B
@�B
A;B
BB
B�B
CaB
C�B
C{B
CaB
C{B
C�B
CGB
C�B
C�B
C�B
C�B
C�B
C�B
C�B
CaB
C-B
CB
C�B
C{B
C�B
C�B
D�B
EB
D�B
ESB
FB
E�B
E�B
E�B
E�B
FtB
FtB
FYB
F�B
G�B
GEB
G_B
G+B
GzB
G�B
G�B
G�B
H�B
H�B
IB
H�B
J=B
J�B
L0B
L~B
M6B
M�B
N<B
N�B
N�B
N�B
O�B
O�B
O�B
P.B
P}B
P�B
QhB
Q4B
Q�B
Q4B
Q�B
Q�B
Q�B
RTB
R:B
SuB
S�B
S�B
TB
T�B
T�B
T�B
T�B
UgB
U�B
VmB
W�B
Y1B
X�B
XEB
XyB
X_B
X�B
X+B
YB
X�B
Y�B
Z�B
[	B
[#B
[	B
[qB
\�B
^�B
^�B
^jB
^B
^�B
_B
_B
_;B
_�B
_pB
`BB
`vB
`�B
`vB
aB
`�B
aHB
a�B
a�B
a�B
a�B
a�B
a|B
b4B
b4B
b�B
b�B
c�B
b�B
c B
cTB
c�B
c�B
d�B
d�B
e`B
e�B
ezB
e�B
e�B
e�B
f�B
e�B
f�B
fB
ffB
f�B
g�B
f�B
g�B
g�B
h�B
h�B
h�B
hsB
i*B
h�B
iDB
i_B
i�B
i�B
i�B
i�B
i�B
i�B
jKB
jeB
j0B
j�B
kQB
kB
k�B
k�B
k�B
l"B
k�B
lWB
l=B
l"B
k�B
l�B
lqB
l�B
l�B
mB
m]B
m�B
m�B
m�B
m�B
nIB
n�B
ncB
n}B
n�B
o�B
o�B
o5B
o B
n�B
oiB
oiB
oB
o�B
o�B
o�B
o�B
o�B
o�B
p!B
p�B
p;B
p!B
p�B
p�B
p�B
qAB
p�B
qB
q[B
qvB
q�B
rGB
r�B
r�B
s�B
shB
sB
r�B
s�B
sMB
s�B
s�B
tTB
t�B
vzB
vzB
vB
v+B
u�B
u�B
u�B
u�B
u�B
uZB
u�B
v`B
vB
w2B
v�B
wLB
w�B
w2B
w�B
w�B
w�B
w�B
xRB
x�B
x�B
x�B
y>B
z*B
yrB
y�B
y�B
z�B
z�B
{dB
{JB
{B
|jB
|�B
}B
|�B
}�B
}�B
}�B
~wB
~�B
~]B
~�B
~�B
.B
cB
}B
�B
�B
�B
�B
�4B
�OB
�4B
�iB
�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104917  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173715  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173716  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173716                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023723  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023723  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                