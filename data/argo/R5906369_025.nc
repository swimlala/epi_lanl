CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-12-09T08:01:23Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  pX   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ʌ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �x   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �X   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �X   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �X   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �X   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20211209080123  20211209080123  5906369 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  8344                            2B  A   NAVIS_A                         1262                            170425                          863 @٨т���1   @٨�)���@8 A�7K��c�����1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^�fD_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D�fD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�	�D�)�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@|��@�  @�  A  A<  A\  A|  A�  A�  A���A�  A�  A�  A�  A�  B  B  B��B  B'  B/  B7  B?  BG  BO  BW  B_  Bg  Bo  Bw  B  B�� B�� B�� B�� B��3B�L�B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BÀ Bǀ Bˀ Bπ BӀ B׀ Bۀ B߀ B� B� B� B� B� B�� B�� B�� C� C� C� C� C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D p D � Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D	p D	� D
p D
� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D p D � D!p D!� D"p D"� D#p D#� D$p D$� D%p D%� D&p D&� D'p D'� D(p D(� D)p D)� D*p D*� D+p D+� D,p D,� D-p D-� D.p D.� D/p D/� D0p D0� D1p D1� D2p D2� D3p D3� D4p D4� D5p D5� D6p D6� D7p D7� D8p D8� D9p D9� D:p D:� D;p D;� D<p D<� D=p D=� D>p D>� D?p D?� D@p D@� DAp DA� DBp DB� DCp DC� DDp DD� DEp DE� DFp DF� DGp DG� DHp DH� DIp DI� DJp DJ� DKp DK� DLp DL� DMp DM� DNp DN� DOp DO� DPp DP� DQp DQ� DRp DR� DSp DS� DTp DT� DUp DU� DVp DV� DWp DW� DXp DX� DYp DY� DZp DZ� D[p D[� D\p D\� D]p D]� D^vfD^� D_p D_� D`p D`� Dap Da� Dbp Db� Dcp Dc� Ddp Dd� Dep De� Dfp Df� Dgp Dg� Dhp Dh� Dip Di� Djp Dj� Dkp Dk� Dlp Dl� Dmp Dm� Dnp Dn� Dop Do� Dpp Dp� Dqp Dq� Drp Dr� Dsp Ds� Dtp Dt� Dup Du� Dvp Dv� Dwp Dw� Dxp Dx� Dyp Dy� Dzp Dz� D{p D{� D|p D|� D}p D}� D~p D~� DvfD� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D¸ D�� D�8 D�x Dø D�� D�8 D�x Dĸ D�� D�8 D�x DŸ D�� D�8 D�x DƸ D�� D�8 D�x DǸ D�� D�8 D�x Dȸ D�� D�8 D�x Dɸ D�� D�8 D�x Dʸ D�� D�8 D�x D˸ D�� D�8 D�x D̸ D�� D�8 D�x D͸ D�� D�8 D�x Dθ D�� D�8 D�x Dϸ D�� D�8 D�x Dи D�� D�8 D�x DѸ D�� D�8 D�x DҸ D�� D�8 D�x DӸ D�� D�8 D�x DԸ D�� D�8 D�x Dո D�� D�8 D�x Dָ D�� D�8 D�x D׸ D�� D�8 D�x Dظ D�� D�8 D�x Dٸ D�� D�8 D�x Dڸ D�� D�8 D�x D۸ D�� D�8 D�x Dܸ D�� D�8 D�x Dݸ D�� D�8 D�x D޸ D�� D�8 D�x D߸ D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D��D�� D�8 D�x D� D�� D�8 D�t�D� D�� D�8 D�x D� D�� D�8 D�x D�� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D��D�!�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��yA��yA��A��A��A��A��A��A���A���A���A���A���A���A���A�A�A�  A���A���A���A���A�A�1A�%A�  A�  A�A���A�%A�JA�  A�|�A��yA�JA��RA���A�|�A�1'A���A�bNA��A�9XA��A� �A���A�I�A��A�/A���A���A��`A�bNA�M�A�M�A��mA�  A� �A��hA��A��7A��;A���A��RA���A��uA��A�E�A��jA��A��A��+A�bA�&�A�VA�oA��HA��A� �A��-A�E�A��mA�G�A�
=A�7LA��RA��A�jA�7LA�oA�XA�1A�  A��7A��mA�JA�C�A���A�A���A��TA�9XA��jA��A��A���A�~�A��yA���A�Q�A��FA�bA~�A|r�A{�Ax(�AtVAs��Ap��Am�AmVAl�AjjAh��Af�RAd^5Aa�-A`$�A_�A^��A^-A\�RAZ�/AXbAV��AVbAU\)AT��AS+APn�AN��AL(�AJ�DAI`BAG�PAG+AF�!AE�AB�DAA��A?�A>��A>�A<ȴA;�A:ȴA9�A9p�A8�A81A6v�A5/A4�DA3�^A2��A1��A133A/��A/
=A.n�A-��A-
=A,bNA+�-A+;dA*�HA*A�A(�HA'ƨA'"�A&�!A%��A$�RA"�HA!��A!A �!A��A+A�TA��A�\A�AXA�`A5?A�7A��AVAM�A  A��A�An�A�
A�AhsA��A�A��A��AffA�#A%A	/A$�A�Av�AbAl�A�A��A�mA7LAbNA?}@���@�X@��w@�b@�J@�p�@�t�@�+@�@��D@�K�@�@�^5@�hs@�Z@睲@�V@��#@��@�C�@��@�7L@ߕ�@��@�V@�bN@�;d@�n�@�p�@���@�n�@��@��@�`B@�Z@�t�@Ѻ^@��@��@�%@�1'@���@˅@��@�v�@ɑh@ț�@Ǿw@ǅ@ư!@�M�@��@���@�p�@���@§�@�x�@���@��F@�;d@��H@��+@���@�(�@�"�@�$�@��@�G�@��@�ƨ@��@�Ĝ@��@�Q�@���@���@�S�@�V@�=q@�&�@��@��F@�v�@��7@�r�@���@�dZ@�
=@���@���@���@�bN@���@�o@��R@��@���@�&�@��/@�Ĝ@���@��@���@��@��@���@�hs@��@�1'@��F@��!@�n�@�@�@���@��/@��m@�+@�
=@��@���@���@��-@�hs@�%@�I�@�ƨ@���@�C�@���@�@��@�Z@��;@��F@��@���@�;d@�ȴ@��\@�M�@���@�G�@�G�@�G�@�G�@�7L@���@���@���@�z�@�bN@�bN@�I�@��@���@���@���@�ff@���@���@��@�?}@�1'@���@��@��@�dZ@�o@���@�ȴ@��H@��@�
=@��@�33@�;d@�S�@�C�@�o@��@��H@��@�ȴ@��R@��+@���@�@�O�@�/@�&�@��@�%@���@��D@�A�@�ƨ@�"�@���@���@��\@�v�@�=q@���@�hs@�/@��@���@�bN@�9X@�1'@�(�@�  @���@��F@��P@�l�@�dZ@�K�@�o@�@���@�ȴ@��+@�M�@�$�@���@��#@�@�@��-@���@���@��h@�G�@��@��`@��u@�j@�  @~�@~5?@}�@|��@|��@|�/@|�@|I�@{ƨ@{C�@z�H@z��@zJ@yhs@y�@x�9@xA�@x  @w�@w|�@w;d@w
=@vȴ@vV@v{@u@u�h@u�h@u�h@u`B@t�/@t��@tZ@t�@s�
@s�@sdZ@s"�@r��@r=q@q�@q��@q�7@qX@pĜ@o�;@o�;@o�@o�;@o�w@o�P@o|�@ol�@n�y@n�@nȴ@n�R@n��@n5?@m��@m�@kƨ@k�m@k��@l1@kt�@j�@j�H@ko@k�@k�@k�@k��@k�@j��@i�#@i�7@ix�@iX@i7L@hr�@g��@f�@e�@e�-@e��@e�@e`B@e`B@e?}@dj@c�@cdZ@cC�@b�H@b�\@bM�@a��@a��@a��@a�7@aX@`��@`��@`bN@_+@^{@]�@]�T@]��@]��@]O�@\��@\j@\�@[�F@[dZ@[C�@["�@[o@Z�\@Z�@Y�@Y��@Y��@Yx�@Yx�@Yx�@YX@X�u@Xb@V�@U�@U�-@U��@U`B@T�/@T�D@S��@SdZ@SS�@S"�@R�\@R~�@RM�@Q��@Qx�@P��@P��@P�@Pb@O|�@O+@N�@N�R@N��@N�+@NV@M�-@MO�@M/@M�@L��@L��@L��@Lj@LI�@L�@K�
@Kƨ@K�F@K��@Kt�@KdZ@KC�@J�@J~�@J=q@I�#@I��@I��@I�^@I��@IG�@H��@HĜ@Hr�@H �@H  @G�;@G�@G+@F�R@F�+@F5?@E�@E�T@E@E��@E/@D��@D��@DI�@C��@C��@C��@C�@C33@C@B��@Bn�@A&�@@�@@1'@@ �@@b@@  @?�;@?�;@?�;@?�P@>�R@>@=��@=��@=/@<�D@<z�@<z�@<(�@;ƨ@;�F@;t�@;33@:��@:�!@:�\@:M�@9�^@9��@9x�@9G�@97L@8��@8�u@8A�@7�;@7��@7|�@7l�@7+@7
=@6�R@5�@5`B@5�@4��@4�/@4�j@4�@4�D@4j@4(�@3ƨ@3S�@2�@2�H@2�H@2��@2~�@1�^@1X@17L@0��@0��@0Ĝ@0�9@0��@0�u@0�u@0�@0r�@0  @/��@/�@/�@/��@/|�@/;d@/�@.ȴ@.�+@.ff@.V@-�@-��@-��@-@-@-@-�-@-��@-p�@,��@,�j@,(�@,1@+�
@+ƨ@+��@+t�@+S�@+C�@+"�@+@*��@*�\@*n�@*n�@*M�@*J@)��@)�#@)�^@)x�@)%@(��@(Ĝ@(r�@(b@'�P@&��@&v�@&E�@&@%�T@%@%��@%O�@%?}@%�@%�@%V@$��@$��@$��@$�@$�D@$j@$I�@$�@#�m@#�m@#�F@#S�@#@"�!@"~�@"M�@"J@!�#@!x�@!&�@!%@ Ĝ@ A�@��@|�@K�@�@
=@��@�@ff@$�@�T@��@p�@?}@�@��@I�@9X@�@��@��@t�@dZ@dZ@S�@C�@"�@@�@��@��@�\@�^@��@x�@7L@Ĝ@bN@A�@b@�@��@�w@��@l�@+@ȴ@�+@ff@$�@�@@�-@��@�h@�h@�h@p�@?}@��@�/@�j@�@��@j@ƨ@��@��@"�@@�@�!@^5@�@�@hs@��@��@��@Ĝ@��@�@A�@�@��@�@�P@\)@�@�y@��@v�@V@@�@�-@p�@?}@V@��@�@�/@��@��@�@z�@j@I�@�@��@ƨ@�F@�F@��@dZ@"�@@@
��@
�\@
~�@
~�@
n�@
n�@
M�@
�@	��@	�@	�#@	��@	hs@	%@��@Ĝ@�u@�@r�@bN@Q�@A�@b@  @�@��@K�@;d@+@
=@ȴ@�R@��@v�@ff@V@E�@@�T@�-@`B@?}@?}@�@�@�j@��@�D@z�@j@j@Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��A��yA��yA��A��A��A��A��A��A���A���A���A���A���A���A���A�A�A�  A���A���A���A���A�A�1A�%A�  A�  A�A���A�%A�JA�  A�|�A��yA�JA��RA���A�|�A�1'A���A�bNA��A�9XA��A� �A���A�I�A��A�/A���A���A��`A�bNA�M�A�M�A��mA�  A� �A��hA��A��7A��;A���A��RA���A��uA��A�E�A��jA��A��A��+A�bA�&�A�VA�oA��HA��A� �A��-A�E�A��mA�G�A�
=A�7LA��RA��A�jA�7LA�oA�XA�1A�  A��7A��mA�JA�C�A���A�A���A��TA�9XA��jA��A��A���A�~�A��yA���A�Q�A��FA�bA~�A|r�A{�Ax(�AtVAs��Ap��Am�AmVAl�AjjAh��Af�RAd^5Aa�-A`$�A_�A^��A^-A\�RAZ�/AXbAV��AVbAU\)AT��AS+APn�AN��AL(�AJ�DAI`BAG�PAG+AF�!AE�AB�DAA��A?�A>��A>�A<ȴA;�A:ȴA9�A9p�A8�A81A6v�A5/A4�DA3�^A2��A1��A133A/��A/
=A.n�A-��A-
=A,bNA+�-A+;dA*�HA*A�A(�HA'ƨA'"�A&�!A%��A$�RA"�HA!��A!A �!A��A+A�TA��A�\A�AXA�`A5?A�7A��AVAM�A  A��A�An�A�
A�AhsA��A�A��A��AffA�#A%A	/A$�A�Av�AbAl�A�A��A�mA7LAbNA?}@���@�X@��w@�b@�J@�p�@�t�@�+@�@��D@�K�@�@�^5@�hs@�Z@睲@�V@��#@��@�C�@��@�7L@ߕ�@��@�V@�bN@�;d@�n�@�p�@���@�n�@��@��@�`B@�Z@�t�@Ѻ^@��@��@�%@�1'@���@˅@��@�v�@ɑh@ț�@Ǿw@ǅ@ư!@�M�@��@���@�p�@���@§�@�x�@���@��F@�;d@��H@��+@���@�(�@�"�@�$�@��@�G�@��@�ƨ@��@�Ĝ@��@�Q�@���@���@�S�@�V@�=q@�&�@��@��F@�v�@��7@�r�@���@�dZ@�
=@���@���@���@�bN@���@�o@��R@��@���@�&�@��/@�Ĝ@���@��@���@��@��@���@�hs@��@�1'@��F@��!@�n�@�@�@���@��/@��m@�+@�
=@��@���@���@��-@�hs@�%@�I�@�ƨ@���@�C�@���@�@��@�Z@��;@��F@��@���@�;d@�ȴ@��\@�M�@���@�G�@�G�@�G�@�G�@�7L@���@���@���@�z�@�bN@�bN@�I�@��@���@���@���@�ff@���@���@��@�?}@�1'@���@��@��@�dZ@�o@���@�ȴ@��H@��@�
=@��@�33@�;d@�S�@�C�@�o@��@��H@��@�ȴ@��R@��+@���@�@�O�@�/@�&�@��@�%@���@��D@�A�@�ƨ@�"�@���@���@��\@�v�@�=q@���@�hs@�/@��@���@�bN@�9X@�1'@�(�@�  @���@��F@��P@�l�@�dZ@�K�@�o@�@���@�ȴ@��+@�M�@�$�@���@��#@�@�@��-@���@���@��h@�G�@��@��`@��u@�j@�  @~�@~5?@}�@|��@|��@|�/@|�@|I�@{ƨ@{C�@z�H@z��@zJ@yhs@y�@x�9@xA�@x  @w�@w|�@w;d@w
=@vȴ@vV@v{@u@u�h@u�h@u�h@u`B@t�/@t��@tZ@t�@s�
@s�@sdZ@s"�@r��@r=q@q�@q��@q�7@qX@pĜ@o�;@o�;@o�@o�;@o�w@o�P@o|�@ol�@n�y@n�@nȴ@n�R@n��@n5?@m��@m�@kƨ@k�m@k��@l1@kt�@j�@j�H@ko@k�@k�@k�@k��@k�@j��@i�#@i�7@ix�@iX@i7L@hr�@g��@f�@e�@e�-@e��@e�@e`B@e`B@e?}@dj@c�@cdZ@cC�@b�H@b�\@bM�@a��@a��@a��@a�7@aX@`��@`��@`bN@_+@^{@]�@]�T@]��@]��@]O�@\��@\j@\�@[�F@[dZ@[C�@["�@[o@Z�\@Z�@Y�@Y��@Y��@Yx�@Yx�@Yx�@YX@X�u@Xb@V�@U�@U�-@U��@U`B@T�/@T�D@S��@SdZ@SS�@S"�@R�\@R~�@RM�@Q��@Qx�@P��@P��@P�@Pb@O|�@O+@N�@N�R@N��@N�+@NV@M�-@MO�@M/@M�@L��@L��@L��@Lj@LI�@L�@K�
@Kƨ@K�F@K��@Kt�@KdZ@KC�@J�@J~�@J=q@I�#@I��@I��@I�^@I��@IG�@H��@HĜ@Hr�@H �@H  @G�;@G�@G+@F�R@F�+@F5?@E�@E�T@E@E��@E/@D��@D��@DI�@C��@C��@C��@C�@C33@C@B��@Bn�@A&�@@�@@1'@@ �@@b@@  @?�;@?�;@?�;@?�P@>�R@>@=��@=��@=/@<�D@<z�@<z�@<(�@;ƨ@;�F@;t�@;33@:��@:�!@:�\@:M�@9�^@9��@9x�@9G�@97L@8��@8�u@8A�@7�;@7��@7|�@7l�@7+@7
=@6�R@5�@5`B@5�@4��@4�/@4�j@4�@4�D@4j@4(�@3ƨ@3S�@2�@2�H@2�H@2��@2~�@1�^@1X@17L@0��@0��@0Ĝ@0�9@0��@0�u@0�u@0�@0r�@0  @/��@/�@/�@/��@/|�@/;d@/�@.ȴ@.�+@.ff@.V@-�@-��@-��@-@-@-@-�-@-��@-p�@,��@,�j@,(�@,1@+�
@+ƨ@+��@+t�@+S�@+C�@+"�@+@*��@*�\@*n�@*n�@*M�@*J@)��@)�#@)�^@)x�@)%@(��@(Ĝ@(r�@(b@'�P@&��@&v�@&E�@&@%�T@%@%��@%O�@%?}@%�@%�@%V@$��@$��@$��@$�@$�D@$j@$I�@$�@#�m@#�m@#�F@#S�@#@"�!@"~�@"M�@"J@!�#@!x�@!&�@!%@ Ĝ@ A�@��@|�@K�@�@
=@��@�@ff@$�@�T@��@p�@?}@�@��@I�@9X@�@��@��@t�@dZ@dZ@S�@C�@"�@@�@��@��@�\@�^@��@x�@7L@Ĝ@bN@A�@b@�@��@�w@��@l�@+@ȴ@�+@ff@$�@�@@�-@��@�h@�h@�h@p�@?}@��@�/@�j@�@��@j@ƨ@��@��@"�@@�@�!@^5@�@�@hs@��@��@��@Ĝ@��@�@A�@�@��@�@�P@\)@�@�y@��@v�@V@@�@�-@p�@?}@V@��@�@�/@��@��@�@z�@j@I�@�@��@ƨ@�F@�F@��@dZ@"�@@@
��@
�\@
~�@
~�@
n�@
n�@
M�@
�@	��@	�@	�#@	��@	hs@	%@��@Ĝ@�u@�@r�@bN@Q�@A�@b@  @�@��@K�@;d@+@
=@ȴ@�R@��@v�@ff@V@E�@@�T@�-@`B@?}@?}@�@�@�j@��@�D@z�@j@j@Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�JB|�Bs�Bw�B{�B}�B{�B{�B� B�B�%B�%B�=B�JB�PB�bB�uB�uB�oB�BjB_;B[#BT�BM�BH�BA�B=qB6FB+B"�B�BuB\B+B��B�B�B�HB�B��B�jB��B��B��B�DB�B~�By�Br�B`BBT�B@�B-B(�B$�B!�B�B%B
�B
�B
�NB
��B
ǮB
�wB
�9B
�B
��B
��B
�\B
�+B
z�B
r�B
r�B
k�B
hsB
dZB
`BB
ZB
Q�B
C�B
:^B
)�B
1B	��B	�B	��B	��B	��B	�}B	�9B	��B	��B	�hB	�%B	�B	}�B	x�B	o�B	dZB	T�B	L�B	I�B	H�B	D�B	B�B	2-B	(�B	�B	�B	hB	
=B	+B	B��B�B�yB�TB�/B�5B�B��BŢB��B�qB�XB�9B�!B��B��B��B��B��B��B��B��B�uB�uB�oB�hB�\B�VB�JB�DB�7B�B�B|�By�Bt�Bo�BjBiyBhsBdZBcTBbNB_;B^5B]/B]/B[#BXBYBT�BR�BP�BO�BO�BN�BL�BL�BI�BG�BE�BD�BC�B@�B?}B<jB<jB:^B9XB8RB7LB7LB6FB5?B49B49B49B49B33B2-B/B.B.B+B)�B)�B'�B&�B&�B%�B+B)�B+B,B,B+B)�B(�B)�B'�B&�B'�B'�B'�B'�B)�B)�B+B.B0!B0!B0!B0!B1'B1'B33B49B5?B6FB7LB7LB8RB9XB9XB;dB=qB>wB>wB?}B?}B?}B>wB>wB@�BB�BD�BE�BF�BG�BG�BG�BH�BI�BI�BH�BH�BI�BJ�BJ�BH�BG�BI�BJ�BJ�BK�BN�BO�BVB[#B[#B]/B^5B`BBbNBe`BgmBgmBffBiyBk�Bm�Bq�Bs�Bt�Bw�By�B~�B�1B�DB�JB�PB�DB�DB�DB�VB�\B�{B�{B��B��B��B��B��B��B��B��B��B��B�!B�'B�?B�wB�wB��BǮBɺB��B��B��B��B�B�/B�BB�HB�BB�BB�NB�mB�B�B�B��B��B��B��B��B��B��B��B	B	B	B	B	B	+B	1B	
=B	PB	PB	hB	{B	�B	�B	�B	�B	�B	�B	#�B	'�B	,B	/B	0!B	2-B	5?B	7LB	9XB	=qB	?}B	A�B	B�B	B�B	C�B	D�B	G�B	J�B	P�B	S�B	YB	ZB	[#B	[#B	\)B	^5B	`BB	aHB	ffB	k�B	m�B	o�B	p�B	q�B	s�B	v�B	z�B	}�B	~�B	�B	�B	�%B	�%B	�%B	�1B	�1B	�=B	�DB	�DB	�DB	�PB	�VB	�VB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�?B	�LB	�RB	�XB	�dB	�jB	�jB	�wB	�}B	��B	��B	B	B	B	ÖB	ŢB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�)B	�;B	�HB	�NB	�TB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
DB
JB
PB
PB
PB
PB
VB
\B
\B
bB
bB
hB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
'�B
(�B
)�B
+B
,B
,B
,B
.B
.B
.B
/B
/B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�JB|�Bs�Bw�B{�B}�B{�B{�B� B�B�%B�%B�=B�JB�PB�bB�uB�uB�oB�BjB_;B[#BT�BM�BH�BA�B=qB6FB+B"�B�BuB\B+B��B�B�B�HB�B��B�jB��B��B��B�DB�B~�By�Br�B`BBT�B@�B-B(�B$�B!�B�B%B
�B
�B
�NB
��B
ǮB
�wB
�9B
�B
��B
��B
�\B
�+B
z�B
r�B
r�B
k�B
hsB
dZB
`BB
ZB
Q�B
C�B
:^B
)�B
1B	��B	�B	��B	��B	��B	�}B	�9B	��B	��B	�hB	�%B	�B	}�B	x�B	o�B	dZB	T�B	L�B	I�B	H�B	D�B	B�B	2-B	(�B	�B	�B	hB	
=B	+B	B��B�B�yB�TB�/B�5B�B��BŢB��B�qB�XB�9B�!B��B��B��B��B��B��B��B��B�uB�uB�oB�hB�\B�VB�JB�DB�7B�B�B|�By�Bt�Bo�BjBiyBhsBdZBcTBbNB_;B^5B]/B]/B[#BXBYBT�BR�BP�BO�BO�BN�BL�BL�BI�BG�BE�BD�BC�B@�B?}B<jB<jB:^B9XB8RB7LB7LB6FB5?B49B49B49B49B33B2-B/B.B.B+B)�B)�B'�B&�B&�B%�B+B)�B+B,B,B+B)�B(�B)�B'�B&�B'�B'�B'�B'�B)�B)�B+B.B0!B0!B0!B0!B1'B1'B33B49B5?B6FB7LB7LB8RB9XB9XB;dB=qB>wB>wB?}B?}B?}B>wB>wB@�BB�BD�BE�BF�BG�BG�BG�BH�BI�BI�BH�BH�BI�BJ�BJ�BH�BG�BI�BJ�BJ�BK�BN�BO�BVB[#B[#B]/B^5B`BBbNBe`BgmBgmBffBiyBk�Bm�Bq�Bs�Bt�Bw�By�B~�B�1B�DB�JB�PB�DB�DB�DB�VB�\B�{B�{B��B��B��B��B��B��B��B��B��B��B�!B�'B�?B�wB�wB��BǮBɺB��B��B��B��B�B�/B�BB�HB�BB�BB�NB�mB�B�B�B��B��B��B��B��B��B��B��B	B	B	B	B	B	+B	1B	
=B	PB	PB	hB	{B	�B	�B	�B	�B	�B	�B	#�B	'�B	,B	/B	0!B	2-B	5?B	7LB	9XB	=qB	?}B	A�B	B�B	B�B	C�B	D�B	G�B	J�B	P�B	S�B	YB	ZB	[#B	[#B	\)B	^5B	`BB	aHB	ffB	k�B	m�B	o�B	p�B	q�B	s�B	v�B	z�B	}�B	~�B	�B	�B	�%B	�%B	�%B	�1B	�1B	�=B	�DB	�DB	�DB	�PB	�VB	�VB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�?B	�LB	�RB	�XB	�dB	�jB	�jB	�wB	�}B	��B	��B	B	B	B	ÖB	ŢB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�)B	�;B	�HB	�NB	�TB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
DB
JB
PB
PB
PB
PB
VB
\B
\B
bB
bB
hB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
'�B
(�B
)�B
+B
,B
,B
,B
.B
.B
.B
/B
/B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20211209080123                              AO  ARCAADJP                                                                    20211209080123    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20211209080123  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20211209080123  QCF$                G�O�G�O�G�O�0               