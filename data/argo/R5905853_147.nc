CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-02-18T03:42:44Z creation;2023-02-18T03:42:45Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230218034244  20230218035841  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @����˪1   @���So�@.��hr�!�c)XbM�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A!��AA��A\��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�33B���B�  B�33B���B�ffB�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C��C�fC�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2L�C3�fC6  C8  C:  C<  C>  C@�CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl�Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�<�D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @33@p  @�  @�  A��A=��AX��A|  A�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B  B'  B/  B7  B?  BG  BO  BW  B_  Bg  BoffBw  B  B�� B�� B�� B��3B��B�� B��3B��B��fB�� B�L�B�L�B�� B�� B�� B�� BÀ Bǀ Bˀ Bπ B��fB׀ Bۀ B߀ B� B� B� B� B� B�� B�� B�� C� C� C� C� C	ٚCY�C�fC�fC� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C2�C3�fC5� C7� C9� C;� C=� C?ٚCA� CC�fCE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce�fCg� Ci� CkٚCmٚCo� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D p D � Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D	p D	� D
p D
� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D p D � D!p D!� D"p D"� D#p D#� D$p D$� D%p D%� D&p D&� D'p D'� D(p D(� D)p D)� D*p D*� D+p D+�fD,p D,� D-p D-� D.p D.� D/p D/� D0p D0� D1p D1� D2p D2� D3p D3� D4p D4� D5p D5� D6p D6� D7p D7� D8p D8� D9p D9� D:p D:� D;p D;� D<p D<� D=p D=� D>p D>� D?p D?� D@p D@� DAp DA� DBp DB� DCp DC� DDp DD� DEp DE� DFp DF� DGp DG� DHp DH� DIp DI� DJp DJ� DKp DK� DLp DL� DMp DM� DNp DN� DOp DO� DPp DP� DQp DQ� DRp DR� DSp DS� DTp DT� DUp DU� DVp DV� DWp DW� DXp DX� DYp DY� DZp DZ� D[p D[� D\p D\� D]p D]� D^p D^� D_p D_� D`p D`� Dap Da� Dbp Db� Dcp Dc� Ddp Dd� Dep De� Dfp Df� Dgp Dg� Dhp Dh� Dip Di� Djp Dj� Dkp Dk� Dlp Dl� Dmp Dm� Dnp Dn� Dop Do� Dpp Dp� Dqp Dq� Drp Dr� Dsp Ds� Dtp Dt� Dup Du� Dvp Dv� Dwp Dw� Dxp Dx� Dyp Dy� Dzp Dz� D{p D{� D|p D|� D}p D}� D~p D~� Dp D� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�;3D�x D��3D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�{3D�� D�� D�4�D�x D�� D��3D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�;3D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D¸ D�� D�8 D�x Dø D�� D�8 D�x Dĸ D�� D�8 D�x DŸ D�� D�8 D�x DƸ D�� D�8 D�x DǸ D�� D�8 D�x Dȸ D�� D�8 D�x Dɸ D�� D�8 D�x Dʸ D�� D�8 D�x D˸ D�� D�8 D�x D̸ D�� D�8 D�x D͸ D�� D�8 D�x Dθ D�� D�8 D�x Dϸ D�� D�8 D�x Dи D�� D�8 D�x DѸ D�� D�8 D�x DҸ D�� D�8 D�x DӸ D�� D�8 D�x DԸ D�� D�8 D�x Dո D�� D�8 D�x Dָ D�� D�8 D�x D׸ D�� D�8 D�x Dظ D�� D�8 D�x Dٸ D�� D�8 D�x Dڸ D�� D�8 D�x D۸ D�� D�8 D�x Dܸ D�� D�8 D�x Dݸ D�� D�8 D�x D޸ D�� D�8 D�x D߸ D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D�� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D���D�8 D�x D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A�SA�A�@A��A��A��A�%FA�(�A�)_A�)_A�'�A�'�A�+�A�+�A�+A�+�A�,qA�-CA�.A�.A�.IA�0!A�1'A�0�A�0�A�2�A�1�A�0UA�1�A�1�A�6FA�;0A�;�A�;dA�&�A��#A�VmA�`BA�IA�a�A��CA��A���A��%A�d�A�;0A��fA��A�	lA��;A��A���A�cTA��UA��uA�kA�	�A���A�E9A�R�A��.A���A�$�A�ҽA�^�A�;0A�PA�j�A�e�A���A��@A�kQA��A��A�{A��{A�8A�.�A�AA���A�A�1A��xA�-�A��A�z�A���Ay(Aq��Ao�zAn��Al�Ai�AfrGA`�qA\˒AV��AN4AJ�XAG�AE��AD%FAA��A@�A?��A>��A:A6��A5C�A3W?A1��A.��A,�PA*�jA)#:A(Z�A'2�A%c A#�A#�A"�WA �OA��ARTAVA+A{JAFAJ�A�mA��AL0A��A�A�DA�oA�DA�A�4A�AxA��A}VA�9AkQAT�A>BAy�AȴA(�A5�A��A �Aw2A�A�A
	lA�A�A,=A�,AzA#:A�zA��A$tA�PA��AQA��A�Ar�A%�A	lA�xA�A ($@��@��Z@�f�@�~�@���@�%�@���@�S&@��8@��Z@���@��@�,�A  \A ;�@�Y�@��@���@���@���@��@�.I@�\�@��P@���@��@��@�l�@��^@��@��]@�ߤ@��@�Z�@�1@��6@�ƨ@�x�@�"�@�ی@�@�z@��&@�Y�@��'@��@�K�@�-@�N<@�-w@�rG@�F@�u�@��@�iD@��@ߓ�@�W?@ށo@��@���@�_@�1@ۊ	@۪�@��g@ۜ�@�*0@��8@ڇ+@٭C@ٞ�@�Mj@�Z�@��o@�X�@�q�@ղ�@�j@�dZ@��@�l�@�H@Ӑ�@�p�@Ҵ9@�@���@�@�~�@�I�@�	�@�^�@�C@�ں@Ίr@�L0@�4n@�4@���@�Z�@�_@���@�u�@�1�@��@��	@���@���@�w�@� �@�A�@��K@��]@�Ɇ@Ƚ<@ȴ9@Ȥ�@ȝI@ș1@Ȏ�@�l"@�خ@�͟@�kQ@�L0@�$@�_@��
@ů�@ŋ�@�m]@�a�@�J�@���@Ě�@�~(@�s�@�A�@��@Ì~@�@¶�@�h�@�.�@�9�@�6�@��@��"@���@��@���@�.I@�ѷ@�Ta@���@��~@�e�@�L�@�@O@�(�@��@��!@�]d@��@��@���@�c�@���@�Dg@�-w@��@���@���@�ff@�� @��_@�^5@�1@�	l@�ں@��m@���@��@�C-@��x@��@���@��m@��@�ں@��@�.�@��@��Z@���@�rG@��@��U@�~�@�$�@��@��h@��@��@�Mj@��@��$@�p;@�"h@�dZ@�7L@�.I@���@���@�H�@�-@��@��q@��v@���@�C-@���@���@�8@��@��'@�n�@�ff@�V�@���@��@�L�@�A @�� @�  @���@��w@���@�U�@�C@���@��@��@�o @�P�@�Y@�Ĝ@�S�@�	@��d@�zx@�Vm@�V@���@���@�?�@���@���@�zx@�X�@�=�@�2a@�(�@�
=@��@��@�3�@���@��q@��n@���@�^�@��@��@�Ta@�I�@�J@���@��X@���@�[W@�%@��F@�s�@�kQ@�=q@��@��@���@�8@���@�PH@�%�@��@���@�|@�ں@�`�@�GE@�*�@�@��@�+@��y@�ȴ@�}V@��Z@��3@���@�u�@�*0@�ߤ@���@�h
@�;�@�	@���@��@��3@��~@�\�@�q@��@��@�ff@�$�@�G@��@�Z�@���@���@�͟@��m@���@��@�oi@�_�@�B[@�)�@�	@���@��@��=@�qv@�RT@�+@�҉@��@��r@�[�@�{@��z@���@�hs@�,�@��'@��@�q�@�S�@�$@���@�ԕ@���@���@�O�@��@��P@��@���@��.@�<�@�1@���@��f@�J�@��2@���@�YK@�}@�@~��@}�@|�@|��@|r�@|'R@|G@{�[@{\)@zQ@y�3@y�7@yhs@x��@w�6@w|�@v�@vq�@u��@u�@u4@t��@t�@s(@r��@r	@q�~@p�@p��@pS�@o�@oj�@o.I@n�@n~�@m�@m�X@m[W@l�@k��@k�4@kY@j��@jL0@j{@j4@iw2@i@h��@h��@h��@h!@g��@gO@f�@e��@e��@e��@e[W@e%F@d��@d?�@d	�@c�@c(@b�]@b�,@b��@b.�@aB�@`��@_ƨ@_;d@_@^�2@^�@^��@^Z�@^4@]�@\G@[�@@[RT@[o@Z�8@Z�'@Z�F@Zl�@Z3�@Y�@Y�@Y}�@Y�@X�@Xy>@XFt@W�Q@W��@W��@WX�@V��@Uԕ@Uq@T��@T�z@T`�@T@S�{@S=@R�@R�F@R6�@Q�.@Q��@Qc�@Q<6@Q!�@Q	l@P��@P��@P[�@O��@O_p@N��@N�A@Mԕ@Mp�@M?}@L��@LFt@K� @KZ�@J�,@J�@J	@I��@I��@I��@H��@H��@H2�@G�K@G�V@G"�@F��@F{�@FO@F	@E��@Ek�@EDg@D��@D�@DZ@C�@C�@B�B@B��@BW�@B5?@B�@B�@B�@A�@A \@@�u@@bN@@K^@?��@?S�@>��@>�h@>�\@>l�@>+k@=�.@=�>@=ԕ@=\�@<�	@<�j@<A�@;�}@;�4@;S�@:�@:ȴ@:��@:�r@:1�@9�.@9��@9s�@9O�@9&�@8�@8�.@8 �@7�@7��@7A�@7o@6�'@6��@6R�@6�@5�@5��@5x�@5!�@5�@4�/@4�O@4tT@4Ft@4~@3�K@3o�@3l�@3"�@3@2�m@28�@1�D@1�d@1�^@1�@1s�@1G�@1%@0֡@0��@0K^@/�;@/��@/'�@/S@.�'@.��@.c @.1�@-�#@-u�@,��@,�@,�e@,N�@+�@+��@+4�@*�H@*Z�@)@)�"@)m]@)�@(��@(~(@(Z@(9X@(G@'��@'��@&�B@&YK@%��@%@%j@$��@$�@$[�@$4n@$@#��@#��@#1�@#$t@#@"�'@"n�@"C�@"C�@"+k@!�@!�C@!rG@!8�@ ��@ Z@ x@��@RT@��@��@��@�R@��@
�@p�@=�@@;@�@��@�@V�@*�@'R@@�@�@RT@o@�@u%@1�@�h@f�@Q�@/@�@%@ѷ@�@��@�?@��@�I@�@��@M@/�@(�@�@�@�{@�@��@�,@�1@a|@^5@Z�@W�@4@�D@�T@��@B�@�@�|@�/@�O@�@m�@~@�+@�Q@�6@˒@�*@dZ@O@@��@�@��@R�@;�@.�@��@/@�@�E@��@H@	�@��@��@y�@l�@]�@o@��@��@�R@��@��@q�@n�@i�@^5@@�@
�@�@��@��@c�@S&@N<@L�@&�@�	@֡@��@<�@�@��@�a@�[@�k@��@�P@P�@
ں@
�x@
s�@
Z�@
GE@
($@	�.@	��@	�@	��@	T�@	F@	2a@	+@�@��@��@�@6@1@��@��@��@t�@]�@"�@��@�c@�@��@��@q�@=q@($11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A�SA�A�@A��A��A��A�%FA�(�A�)_A�)_A�'�A�'�A�+�A�+�A�+A�+�A�,qA�-CA�.A�.A�.IA�0!A�1'A�0�A�0�A�2�A�1�A�0UA�1�A�1�A�6FA�;0A�;�A�;dA�&�A��#A�VmA�`BA�IA�a�A��CA��A���A��%A�d�A�;0A��fA��A�	lA��;A��A���A�cTA��UA��uA�kA�	�A���A�E9A�R�A��.A���A�$�A�ҽA�^�A�;0A�PA�j�A�e�A���A��@A�kQA��A��A�{A��{A�8A�.�A�AA���A�A�1A��xA�-�A��A�z�A���Ay(Aq��Ao�zAn��Al�Ai�AfrGA`�qA\˒AV��AN4AJ�XAG�AE��AD%FAA��A@�A?��A>��A:A6��A5C�A3W?A1��A.��A,�PA*�jA)#:A(Z�A'2�A%c A#�A#�A"�WA �OA��ARTAVA+A{JAFAJ�A�mA��AL0A��A�A�DA�oA�DA�A�4A�AxA��A}VA�9AkQAT�A>BAy�AȴA(�A5�A��A �Aw2A�A�A
	lA�A�A,=A�,AzA#:A�zA��A$tA�PA��AQA��A�Ar�A%�A	lA�xA�A ($@��@��Z@�f�@�~�@���@�%�@���@�S&@��8@��Z@���@��@�,�A  \A ;�@�Y�@��@���@���@���@��@�.I@�\�@��P@���@��@��@�l�@��^@��@��]@�ߤ@��@�Z�@�1@��6@�ƨ@�x�@�"�@�ی@�@�z@��&@�Y�@��'@��@�K�@�-@�N<@�-w@�rG@�F@�u�@��@�iD@��@ߓ�@�W?@ށo@��@���@�_@�1@ۊ	@۪�@��g@ۜ�@�*0@��8@ڇ+@٭C@ٞ�@�Mj@�Z�@��o@�X�@�q�@ղ�@�j@�dZ@��@�l�@�H@Ӑ�@�p�@Ҵ9@�@���@�@�~�@�I�@�	�@�^�@�C@�ں@Ίr@�L0@�4n@�4@���@�Z�@�_@���@�u�@�1�@��@��	@���@���@�w�@� �@�A�@��K@��]@�Ɇ@Ƚ<@ȴ9@Ȥ�@ȝI@ș1@Ȏ�@�l"@�خ@�͟@�kQ@�L0@�$@�_@��
@ů�@ŋ�@�m]@�a�@�J�@���@Ě�@�~(@�s�@�A�@��@Ì~@�@¶�@�h�@�.�@�9�@�6�@��@��"@���@��@���@�.I@�ѷ@�Ta@���@��~@�e�@�L�@�@O@�(�@��@��!@�]d@��@��@���@�c�@���@�Dg@�-w@��@���@���@�ff@�� @��_@�^5@�1@�	l@�ں@��m@���@��@�C-@��x@��@���@��m@��@�ں@��@�.�@��@��Z@���@�rG@��@��U@�~�@�$�@��@��h@��@��@�Mj@��@��$@�p;@�"h@�dZ@�7L@�.I@���@���@�H�@�-@��@��q@��v@���@�C-@���@���@�8@��@��'@�n�@�ff@�V�@���@��@�L�@�A @�� @�  @���@��w@���@�U�@�C@���@��@��@�o @�P�@�Y@�Ĝ@�S�@�	@��d@�zx@�Vm@�V@���@���@�?�@���@���@�zx@�X�@�=�@�2a@�(�@�
=@��@��@�3�@���@��q@��n@���@�^�@��@��@�Ta@�I�@�J@���@��X@���@�[W@�%@��F@�s�@�kQ@�=q@��@��@���@�8@���@�PH@�%�@��@���@�|@�ں@�`�@�GE@�*�@�@��@�+@��y@�ȴ@�}V@��Z@��3@���@�u�@�*0@�ߤ@���@�h
@�;�@�	@���@��@��3@��~@�\�@�q@��@��@�ff@�$�@�G@��@�Z�@���@���@�͟@��m@���@��@�oi@�_�@�B[@�)�@�	@���@��@��=@�qv@�RT@�+@�҉@��@��r@�[�@�{@��z@���@�hs@�,�@��'@��@�q�@�S�@�$@���@�ԕ@���@���@�O�@��@��P@��@���@��.@�<�@�1@���@��f@�J�@��2@���@�YK@�}@�@~��@}�@|�@|��@|r�@|'R@|G@{�[@{\)@zQ@y�3@y�7@yhs@x��@w�6@w|�@v�@vq�@u��@u�@u4@t��@t�@s(@r��@r	@q�~@p�@p��@pS�@o�@oj�@o.I@n�@n~�@m�@m�X@m[W@l�@k��@k�4@kY@j��@jL0@j{@j4@iw2@i@h��@h��@h��@h!@g��@gO@f�@e��@e��@e��@e[W@e%F@d��@d?�@d	�@c�@c(@b�]@b�,@b��@b.�@aB�@`��@_ƨ@_;d@_@^�2@^�@^��@^Z�@^4@]�@\G@[�@@[RT@[o@Z�8@Z�'@Z�F@Zl�@Z3�@Y�@Y�@Y}�@Y�@X�@Xy>@XFt@W�Q@W��@W��@WX�@V��@Uԕ@Uq@T��@T�z@T`�@T@S�{@S=@R�@R�F@R6�@Q�.@Q��@Qc�@Q<6@Q!�@Q	l@P��@P��@P[�@O��@O_p@N��@N�A@Mԕ@Mp�@M?}@L��@LFt@K� @KZ�@J�,@J�@J	@I��@I��@I��@H��@H��@H2�@G�K@G�V@G"�@F��@F{�@FO@F	@E��@Ek�@EDg@D��@D�@DZ@C�@C�@B�B@B��@BW�@B5?@B�@B�@B�@A�@A \@@�u@@bN@@K^@?��@?S�@>��@>�h@>�\@>l�@>+k@=�.@=�>@=ԕ@=\�@<�	@<�j@<A�@;�}@;�4@;S�@:�@:ȴ@:��@:�r@:1�@9�.@9��@9s�@9O�@9&�@8�@8�.@8 �@7�@7��@7A�@7o@6�'@6��@6R�@6�@5�@5��@5x�@5!�@5�@4�/@4�O@4tT@4Ft@4~@3�K@3o�@3l�@3"�@3@2�m@28�@1�D@1�d@1�^@1�@1s�@1G�@1%@0֡@0��@0K^@/�;@/��@/'�@/S@.�'@.��@.c @.1�@-�#@-u�@,��@,�@,�e@,N�@+�@+��@+4�@*�H@*Z�@)@)�"@)m]@)�@(��@(~(@(Z@(9X@(G@'��@'��@&�B@&YK@%��@%@%j@$��@$�@$[�@$4n@$@#��@#��@#1�@#$t@#@"�'@"n�@"C�@"C�@"+k@!�@!�C@!rG@!8�@ ��@ Z@ x@��@RT@��@��@��@�R@��@
�@p�@=�@@;@�@��@�@V�@*�@'R@@�@�@RT@o@�@u%@1�@�h@f�@Q�@/@�@%@ѷ@�@��@�?@��@�I@�@��@M@/�@(�@�@�@�{@�@��@�,@�1@a|@^5@Z�@W�@4@�D@�T@��@B�@�@�|@�/@�O@�@m�@~@�+@�Q@�6@˒@�*@dZ@O@@��@�@��@R�@;�@.�@��@/@�@�E@��@H@	�@��@��@y�@l�@]�@o@��@��@�R@��@��@q�@n�@i�@^5@@�@
�@�@��@��@c�@S&@N<@L�@&�@�	@֡@��@<�@�@��@�a@�[@�k@��@�P@P�@
ں@
�x@
s�@
Z�@
GE@
($@	�.@	��@	�@	��@	T�@	F@	2a@	+@�@��@��@�@6@1@��@��@��@t�@]�@"�@��@�c@�@��@��@q�@=q@($11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	k�B	k�B	k�B	k�B	kQB	k�B	k�B	mB	m�B	m�B	mCB	l�B	l�B	m�B	m�B	mB	m]B	m)B	m]B	m)B	mwB	mwB	mB	l�B	mCB	m�B	m]B	ncB	n�B	n�B	q�B	t�B	x�B	y�B	|B	��B	�RB
N"B
��B
��B
�zB
�"B
�xB
�pB
�5B
�B
�BBoBB�BBHB�B0B
�dB
�tB
��B
�(B
�B
��B
�IB
��B
�B
��B
�RB
��B
�+B
x�B
c�B
X�B
WsB
W�B
\B
h�B
d�B
U�B
L�B
?HB
&�B
�B
JB
  B	�fB	�B	�B	��B	��B	��B	��B	��B	wfB	oiB	cTB	UMB	?�B	�B	�B�"B��B��B�nB�TB�4B�NB��B��B�CB�-B�B��B��B�"B�B�?B��B�OB�_B�<B��B�LB	3B	DB	�B	vB	�B	�B	pB	%�B	!�B	�B	�B	�B	xB	;B	 �B	&B	,�B	2aB	7�B	=�B	@�B	C-B	GEB	T�B	e`B	^�B	V�B	Q B	ZB	c�B	U�B	\�B	b4B	[#B	S@B	M�B	N�B	X�B	V�B	R�B	PB	M�B	MjB	KDB	H�B	IB	M�B	PbB	Q�B	U�B	X_B	W�B	V�B	T�B	T�B	TaB	S&B	P�B	R�B	S&B	S�B	WYB	[�B	g�B	l�B	kQB	m]B	mwB	l�B	x�B	��B	�=B	�B	�WB	��B	�B	�B	�qB	�"B	��B	�5B	��B	�WB	��B	�B	�hB	�>B	��B	�tB	�FB	��B	�xB	��B	�zB	��B	��B	�LB	�zB	�	B	��B	�0B	��B	��B	�IB	�B	�LB	��B	��B	�B	��B	��B	��B	�3B	��B	��B	��B	�/B	�B	��B	�oB	��B	��B	��B	�DB	�B	��B	��B	�wB	��B	� B	��B	��B	��B	ĶB	��B	��B	��B	��B	�aB	ðB	��B	�9B	��B	ƨB	ǔB	�B	�RB	ɆB	�7B	ɆB	�#B	�#B	�rB	��B	��B	��B	�B	��B	˒B	̘B	͟B	�B	οB	�BB	�\B	ϑB	�.B	�bB	�4B	ңB	�B	�B	�uB	�uB	ӏB	ӏB	ӏB	�[B	�@B	�B	ԕB	�aB	�{B	ԕB	�2B	��B	�sB	�B	��B	�xB	ݘB	�B	�@B	�fB	�B	�RB	�sB	��B	��B	��B	�B	�B	��B	�QB	��B	�B	�eB	��B	�B	�0B	�KB	��B	�B	�B	�B	�"B	�B	�WB	�WB	�B	��B	�B	��B	�B	�KB	�B	��B	�B	�KB	��B	�0B	��B	��B	�B	��B	��B	��B	�B	�GB	��B	��B	��B	�B	�B
B
�B
�B
�B
B
�B
mB
mB
�B
�B
�B
�B
B
�B
%B
�B
�B
�B
_B
�B
gB
�B
�B
�B
�B
�B
�B
�B
MB
�B
�B
�B
�B
�B
?B
+B
1B
�B
	�B

#B

XB

rB

=B

=B

�B
B
B
�B
�B
DB
�B
�B
0B
~B
JB
6B
dB
�B
<B
"B
�B
�B
vB
�B
BB
B
vB
BB
bB
�B
�B
B
B
�B
�B
hB
B
�B
�B
TB
oB
[B
uB
{B
�B
�B
B
�B
�B
gB
�B
�B
�B
�B
7B
�B
�B
�B
�B
�B
�B
B
5B
B
�B
�B
B
�B
!B
pB
pB
!B
 �B
 �B
 �B
 �B
!�B
!|B
!�B
!�B
#TB
#�B
#�B
#�B
#nB
$tB
%zB
%�B
&LB
'8B
&�B
'8B
&�B
&�B
'�B
'mB
'�B
'�B
'�B
(�B
)_B
(�B
)_B
*KB
)�B
)�B
*B
)�B
*0B
*B
*B
)�B
*�B
+6B
+B
+B
+�B
+�B
+�B
+�B
,"B
+�B
,WB
,WB
,WB
-CB
./B
./B
.B
/ B
.�B
.�B
/5B
/B
/�B
/�B
/�B
0;B
0�B
1'B
1�B
0�B
0�B
0�B
1�B
2|B
2-B
2�B
2�B
3�B
3�B
3�B
4TB
4�B
4�B
4�B
5?B
5�B
5�B
5�B
5�B
4�B
5%B
5ZB
6B
5�B
5�B
6`B
6�B
6�B
6�B
6FB
5�B
5�B
5�B
6�B
6`B
5�B
5�B
4�B
5%B
5B
6�B
6�B
6�B
6�B
7LB
7�B
7�B
8�B
9$B
:B
:*B
:*B
:*B
:*B
:�B
:�B
:DB
9�B
:xB
;0B
;B
;dB
;�B
;�B
<jB
="B
=�B
>�B
?B
>�B
?.B
?�B
?�B
?HB
?�B
@�B
A�B
A B
A;B
A;B
AUB
A�B
BuB
DB
EB
D�B
D�B
D�B
D�B
EB
E9B
E9B
F�B
F%B
E9B
E�B
H1B
HKB
H�B
HKB
H�B
I�B
I�B
I7B
IB
I�B
I�B
JXB
J#B
K^B
J�B
J�B
K^B
K�B
LB
LJB
L0B
K�B
K�B
LdB
L�B
MB
MB
MjB
M�B
N<B
NpB
NVB
N"B
NpB
OB
O(B
O\B
OBB
O�B
P�B
QhB
QNB
Q�B
RTB
R:B
SB
SB
S�B
S�B
S�B
TFB
T�B
T�B
T�B
T�B
U�B
U�B
VSB
V�B
VmB
V�B
V�B
V�B
WYB
W$B
W�B
W�B
W�B
W�B
W�B
XEB
X�B
YeB
Y�B
Y�B
Z7B
ZB
Z�B
Y�B
ZB
Z�B
ZQB
[�B
Z�B
[#B
[�B
\)B
\]B
\]B
\�B
\�B
\�B
\�B
\�B
\�B
]/B
]~B
]�B
]�B
^jB
^�B
^�B
_VB
_pB
_VB
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
`�B
a�B
a�B
bB
b�B
b�B
cB
c�B
c�B
dB
d&B
d@B
d@B
dtB
dZB
d�B
d�B
e,B
d�B
e�B
e�B
e�B
e,B
e�B
e�B
e�B
f�B
f�B
gRB
gB
g�B
h$B
h>B
hsB
hsB
i*B
i�B
h�B
iB
jKB
jeB
j�B
j�B
j�B
j�B
j�B
kB
k�B
k6B
kkB
k�B
k�B
l"B
l�B
l�B
l�B
mCB
mCB
m]B
m�B
m�B
m�B
nB
m�B
nIB
nIB
ncB
oB
oiB
o�B
oiB
o�B
p!B
p�B
p�B
p�B
q'B
q[B
q�B
rB
rB
rB
r-B
r�B
r�B
r|B
r|B
r�B
r�B
sB
s3B
s�B
s�B
tB
tTB
t�B
t�B
uB
u%B
uB
t�B
u�B
v+B
vzB
vzB
v�B
v�B
v�B
v�B
wLB
wB
wLB
wLB
w2B
w�B
xB
w�B
xlB
x�B
y	B
y�B
y�B
y�B
zB
y�B
z*B
z^B
zxB
zxB
z^B
z�B
z�B
zxB
zxB
z�B
z�B
z�B
z�B
{B
{B
{�B
|B
|B
|PB
|�B
|jB
|�B
|jB
|�B
|�B
|�B
}B
}�B
}�B
}�B
}�B
}�B
}�B
~(B
~�B
~�B
~�B
~�B
~�B
~�B
.B
B
}B
�B
�B
�B
�B
�B
�B
��B
� B
�;B
�UB
��B
��B
�B
�AB
�uB
��B
��B
��B
�B
�GB
�{B
�aB
�{B
��B
��B
��B
��B
��B
��B
�B
�gB
�gB
��B
��B
��B
��B
��B
��B
�9B
�9B
�mB
��B
�B
�YB
�tB
�tB
��B
��B
��B
��B
�zB
��B
��B
��B
��B
�B
�KB
��B
��B
��B
�B
��B
�B
�B
�7B
��B
�lB
��B
�#B
�=B
��B
��B
��B
��B
��B
�^B
�xB
�DB
�DB
��B
��B
��B
�0B
�J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	k�B	k�B	k�B	k�B	kQB	k�B	k�B	mB	m�B	m�B	mCB	l�B	l�B	m�B	m�B	mB	m]B	m)B	m]B	m)B	mwB	mwB	mB	l�B	mCB	m�B	m]B	ncB	n�B	n�B	q�B	t�B	x�B	y�B	|B	��B	�RB
N"B
��B
��B
�zB
�"B
�xB
�pB
�5B
�B
�BBoBB�BBHB�B0B
�dB
�tB
��B
�(B
�B
��B
�IB
��B
�B
��B
�RB
��B
�+B
x�B
c�B
X�B
WsB
W�B
\B
h�B
d�B
U�B
L�B
?HB
&�B
�B
JB
  B	�fB	�B	�B	��B	��B	��B	��B	��B	wfB	oiB	cTB	UMB	?�B	�B	�B�"B��B��B�nB�TB�4B�NB��B��B�CB�-B�B��B��B�"B�B�?B��B�OB�_B�<B��B�LB	3B	DB	�B	vB	�B	�B	pB	%�B	!�B	�B	�B	�B	xB	;B	 �B	&B	,�B	2aB	7�B	=�B	@�B	C-B	GEB	T�B	e`B	^�B	V�B	Q B	ZB	c�B	U�B	\�B	b4B	[#B	S@B	M�B	N�B	X�B	V�B	R�B	PB	M�B	MjB	KDB	H�B	IB	M�B	PbB	Q�B	U�B	X_B	W�B	V�B	T�B	T�B	TaB	S&B	P�B	R�B	S&B	S�B	WYB	[�B	g�B	l�B	kQB	m]B	mwB	l�B	x�B	��B	�=B	�B	�WB	��B	�B	�B	�qB	�"B	��B	�5B	��B	�WB	��B	�B	�hB	�>B	��B	�tB	�FB	��B	�xB	��B	�zB	��B	��B	�LB	�zB	�	B	��B	�0B	��B	��B	�IB	�B	�LB	��B	��B	�B	��B	��B	��B	�3B	��B	��B	��B	�/B	�B	��B	�oB	��B	��B	��B	�DB	�B	��B	��B	�wB	��B	� B	��B	��B	��B	ĶB	��B	��B	��B	��B	�aB	ðB	��B	�9B	��B	ƨB	ǔB	�B	�RB	ɆB	�7B	ɆB	�#B	�#B	�rB	��B	��B	��B	�B	��B	˒B	̘B	͟B	�B	οB	�BB	�\B	ϑB	�.B	�bB	�4B	ңB	�B	�B	�uB	�uB	ӏB	ӏB	ӏB	�[B	�@B	�B	ԕB	�aB	�{B	ԕB	�2B	��B	�sB	�B	��B	�xB	ݘB	�B	�@B	�fB	�B	�RB	�sB	��B	��B	��B	�B	�B	��B	�QB	��B	�B	�eB	��B	�B	�0B	�KB	��B	�B	�B	�B	�"B	�B	�WB	�WB	�B	��B	�B	��B	�B	�KB	�B	��B	�B	�KB	��B	�0B	��B	��B	�B	��B	��B	��B	�B	�GB	��B	��B	��B	�B	�B
B
�B
�B
�B
B
�B
mB
mB
�B
�B
�B
�B
B
�B
%B
�B
�B
�B
_B
�B
gB
�B
�B
�B
�B
�B
�B
�B
MB
�B
�B
�B
�B
�B
?B
+B
1B
�B
	�B

#B

XB

rB

=B

=B

�B
B
B
�B
�B
DB
�B
�B
0B
~B
JB
6B
dB
�B
<B
"B
�B
�B
vB
�B
BB
B
vB
BB
bB
�B
�B
B
B
�B
�B
hB
B
�B
�B
TB
oB
[B
uB
{B
�B
�B
B
�B
�B
gB
�B
�B
�B
�B
7B
�B
�B
�B
�B
�B
�B
B
5B
B
�B
�B
B
�B
!B
pB
pB
!B
 �B
 �B
 �B
 �B
!�B
!|B
!�B
!�B
#TB
#�B
#�B
#�B
#nB
$tB
%zB
%�B
&LB
'8B
&�B
'8B
&�B
&�B
'�B
'mB
'�B
'�B
'�B
(�B
)_B
(�B
)_B
*KB
)�B
)�B
*B
)�B
*0B
*B
*B
)�B
*�B
+6B
+B
+B
+�B
+�B
+�B
+�B
,"B
+�B
,WB
,WB
,WB
-CB
./B
./B
.B
/ B
.�B
.�B
/5B
/B
/�B
/�B
/�B
0;B
0�B
1'B
1�B
0�B
0�B
0�B
1�B
2|B
2-B
2�B
2�B
3�B
3�B
3�B
4TB
4�B
4�B
4�B
5?B
5�B
5�B
5�B
5�B
4�B
5%B
5ZB
6B
5�B
5�B
6`B
6�B
6�B
6�B
6FB
5�B
5�B
5�B
6�B
6`B
5�B
5�B
4�B
5%B
5B
6�B
6�B
6�B
6�B
7LB
7�B
7�B
8�B
9$B
:B
:*B
:*B
:*B
:*B
:�B
:�B
:DB
9�B
:xB
;0B
;B
;dB
;�B
;�B
<jB
="B
=�B
>�B
?B
>�B
?.B
?�B
?�B
?HB
?�B
@�B
A�B
A B
A;B
A;B
AUB
A�B
BuB
DB
EB
D�B
D�B
D�B
D�B
EB
E9B
E9B
F�B
F%B
E9B
E�B
H1B
HKB
H�B
HKB
H�B
I�B
I�B
I7B
IB
I�B
I�B
JXB
J#B
K^B
J�B
J�B
K^B
K�B
LB
LJB
L0B
K�B
K�B
LdB
L�B
MB
MB
MjB
M�B
N<B
NpB
NVB
N"B
NpB
OB
O(B
O\B
OBB
O�B
P�B
QhB
QNB
Q�B
RTB
R:B
SB
SB
S�B
S�B
S�B
TFB
T�B
T�B
T�B
T�B
U�B
U�B
VSB
V�B
VmB
V�B
V�B
V�B
WYB
W$B
W�B
W�B
W�B
W�B
W�B
XEB
X�B
YeB
Y�B
Y�B
Z7B
ZB
Z�B
Y�B
ZB
Z�B
ZQB
[�B
Z�B
[#B
[�B
\)B
\]B
\]B
\�B
\�B
\�B
\�B
\�B
\�B
]/B
]~B
]�B
]�B
^jB
^�B
^�B
_VB
_pB
_VB
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
`�B
a�B
a�B
bB
b�B
b�B
cB
c�B
c�B
dB
d&B
d@B
d@B
dtB
dZB
d�B
d�B
e,B
d�B
e�B
e�B
e�B
e,B
e�B
e�B
e�B
f�B
f�B
gRB
gB
g�B
h$B
h>B
hsB
hsB
i*B
i�B
h�B
iB
jKB
jeB
j�B
j�B
j�B
j�B
j�B
kB
k�B
k6B
kkB
k�B
k�B
l"B
l�B
l�B
l�B
mCB
mCB
m]B
m�B
m�B
m�B
nB
m�B
nIB
nIB
ncB
oB
oiB
o�B
oiB
o�B
p!B
p�B
p�B
p�B
q'B
q[B
q�B
rB
rB
rB
r-B
r�B
r�B
r|B
r|B
r�B
r�B
sB
s3B
s�B
s�B
tB
tTB
t�B
t�B
uB
u%B
uB
t�B
u�B
v+B
vzB
vzB
v�B
v�B
v�B
v�B
wLB
wB
wLB
wLB
w2B
w�B
xB
w�B
xlB
x�B
y	B
y�B
y�B
y�B
zB
y�B
z*B
z^B
zxB
zxB
z^B
z�B
z�B
zxB
zxB
z�B
z�B
z�B
z�B
{B
{B
{�B
|B
|B
|PB
|�B
|jB
|�B
|jB
|�B
|�B
|�B
}B
}�B
}�B
}�B
}�B
}�B
}�B
~(B
~�B
~�B
~�B
~�B
~�B
~�B
.B
B
}B
�B
�B
�B
�B
�B
�B
��B
� B
�;B
�UB
��B
��B
�B
�AB
�uB
��B
��B
��B
�B
�GB
�{B
�aB
�{B
��B
��B
��B
��B
��B
��B
�B
�gB
�gB
��B
��B
��B
��B
��B
��B
�9B
�9B
�mB
��B
�B
�YB
�tB
�tB
��B
��B
��B
��B
�zB
��B
��B
��B
��B
�B
�KB
��B
��B
��B
�B
��B
�B
�B
�7B
��B
�lB
��B
�#B
�=B
��B
��B
��B
��B
��B
�^B
�xB
�DB
�DB
��B
��B
��B
�0B
�J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230218034244  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230218034244  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230218034245  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230218034245                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230218034246  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230218034246  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230218035841                      G�O�G�O�G�O�                