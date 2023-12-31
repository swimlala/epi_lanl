CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-10-30T00:35:18Z creation;2016-10-30T00:35:20Z conversion to V3.1;2019-12-19T08:26:41Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        x  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \l   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  `L   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ̜   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �(   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20161030003518  20200115111518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               4A   JA  I2_0576_052                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��6Z��1   @��7 @:��6���d��g��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`ffBh  Bp  Bw��B��B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� DkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՃ3D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D�� D�  D�@ D��3D��3D�  D�@ D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @vff@�  @�  A  A<  A\  A|  A�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B  B'  B/  B7  B?  BG  BO  BWffB_ffBg  Bo  Bv��B~��B�� B�� B�� B�� B�� B��3B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BÀ Bǀ Bˀ Bπ BӀ B׀ Bۀ B߀ B� B� B� B� B� B�� B�� B�� C� C� C� C� C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D p D � Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D	p D	� D
p D
� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D p D � D!p D!� D"p D"� D#p D#� D$p D$� D%p D%� D&p D&� D'p D'� D(p D(� D)p D)� D*p D*� D+p D+� D,p D,� D-p D-� D.p D.� D/p D/� D0p D0� D1p D1� D2p D2� D3p D3� D4p D4� D5p D5� D6p D6� D7p D7� D8p D8� D9p D9� D:p D:� D;p D;� D<p D<� D=p D=� D>p D>� D?p D?� D@p D@� DAp DA� DBp DB� DCp DC� DDp DD� DEp DE� DFp DF� DGp DG� DHp DH� DIp DI� DJp DJ� DKp DK� DLp DL� DMp DM� DNp DN� DOp DO� DPp DP� DQp DQ� DRp DR� DSp DS� DTp DT� DUp DU� DVp DV� DWp DW� DXp DX� DYp DY� DZp DZ� D[p D[� D\p D\� D]p D]� D^p D^� D_p D_� D`p D`� Dap Da� Dbp Db� Dcp Dc� Ddp Dd� Dep De� Dfp Df� Dgp Dg� Dhp Dh� Dip Di� Djp Dj�fDkp Dk� Dlp Dl� Dmp Dm� Dnp Dn� Dop Do� Dpp Dp� Dqp Dq� Drp Dr� Dsp Ds� Dtp Dt� Dup Du� Dvp Dv� Dwp Dw� Dxp Dx� Dyp Dy� Dzp Dz� D{p D{� D|p D|� D}p D}� D~p D~� Dp D� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D���D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D¸ D�� D�8 D�x Dø D�� D�8 D�x Dĸ D�� D�8 D�x DŸ D�� D�8 D�x DƸ D�� D�8 D�x DǸ D�� D�8 D�x Dȸ D�� D�8 D�x Dɸ D�� D�8 D�x Dʸ D�� D�8 D�x D˸ D�� D�8 D�x D̸ D�� D�8 D�x D͸ D�� D�8 D�x Dθ D�� D�8 D�x Dϸ D�� D�8 D�x Dи D�� D�8 D�x DѸ D�� D�8 D�x DҸ D�� D�8 D�x DӸ D�� D�8 D�x DԸ D�� D�8 D�{3Dո D�� D�8 D�x Dָ D�� D�8 D�x D׸ D�� D�8 D�x Dظ D�� D�8 D�x Dٸ D��3D�;3D�x Dڸ D�� D�8 D�x D۸ D�� D�8 D�x Dܸ D�� D�8 D�x Dݸ D�� D�8 D�x D޸ D�� D�8 D�x D߸ D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D�� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D�� D�� D�8 D�{3D�� D�� D�8 D�{3D��3D�� D�8 D�{3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A���A���A���A���A���A���A��
A��
A��
A��A��A��A��#A��#A��#A��#A��/A��/A��/A��/A��;A��;A��HA��HA��;A�ĜA�=qA�v�A��#A�7LAġ�AÃA�A���A���A��A�1'A�E�A��A�S�A��HA���A��A��FA�A���A��-A���A�A��A�VA�?}A��uA�n�A���A���A�v�A��A�M�A�-A�$�A�1'A��A��FA�(�A�p�A��mA�C�A�VA��
A���A�1'A�?}A�=qA���A�A�&�A��A��7A�7LA�JA�M�A�E�A��A�  A��HA�bAK�A|�AzȴAxA�AwdZAwC�Av�Au��At�uAsS�Ar~�Aq�#Aq�-Aql�Ap�Ao�AoAn��An�9An�+An=qAm�7Alv�Al{Akl�AjȴAj�Ai\)Ah�+AhbAg�;AgƨAg�FAg�-Ag��Af�Af�Ae�7Ae&�Adz�AcO�Abz�A`bA^�!A]��A]\)AZ�AY%AW��AV��AU�
ATARM�AQ�AO;dANȴAN��AM�mAMG�AKK�AHZAGx�AFQ�AD��AC��AB��AB��AB-AA�A@��A@��A@ffA?�#A?%A>-A>1A=��A=G�A<�\A;��A:�A9�A8�A8�`A8��A7"�A6I�A5��A4��A3�TA3�A2�RA0�HA0=qA/��A/�A.��A-�A, �A+A*ZA)��A)��A(�jA'��A&��A%��A%&�A$�9A#��A#A"�DA!��A!;dA!
=A �+Ap�A�A�A��AVA�-A�
AG�A&�A��A-A33A�A��AA�A/A��A�wA�A�HAȴA��AI�A�^AffA��A|�A
��A
�+A
A	|�A	�AI�A/AbNA{Ax�A�RAVA(�A��A
=A��A�\AffAQ�AE�A=qAbA��AVA bN@�@�S�@��\@�z�@�K�@�^5@�Ĝ@�7@�@�J@�j@���@�dZ@�-@�Ĝ@�"�@��/@��
@�C�@◍@��@�33@�@��`@ۍP@��`@�K�@֟�@Լj@҇+@�O�@�b@�+@θR@�M�@�p�@ʏ\@ɑh@�x�@Ɵ�@���@�z�@�(�@��@��@�\)@���@��7@�Z@�t�@�-@�{@�@���@�V@��/@��P@�hs@�|�@�&�@���@���@�@��-@��^@�@���@���@���@��-@�?}@���@��@�A�@���@�K�@�{@�hs@���@�1'@��P@�K�@��y@��-@�V@�j@�C�@���@���@��^@�`B@��@�  @��
@��@���@�dZ@�"�@���@�v�@�J@��7@���@�bN@�Q�@�A�@��@��m@�dZ@�"�@�@�ȴ@�~�@�5?@���@��h@�`B@�?}@��@��@���@��/@�A�@��F@�l�@�K�@�C�@�
=@��R@�-@�p�@���@��D@�1'@�1@��m@���@��P@�K�@�-@��T@��^@���@��7@��@��9@��9@���@���@��j@� �@���@��R@��#@��^@���@�7L@��@�z�@�(�@�  @�ƨ@��@�S�@��@�V@�=q@�=q@�-@��@���@��9@�bN@�  @�ƨ@�|�@�S�@�;d@���@��\@�v�@�E�@�@���@�G�@�&�@��`@���@��9@���@�z�@�Q�@�A�@�b@+@~�+@~@~@}�T@}�-@|�@{�m@{S�@{C�@{o@z�@z��@zJ@y�#@y��@y��@yX@x�`@xĜ@x�u@xbN@x1'@w��@w+@vȴ@v�+@v@u��@uV@t��@s�m@s��@s"�@r��@r=q@q�@q��@qhs@p��@p��@pr�@p �@pb@p  @p�@o�@o��@o;d@nv�@m�@l�@l9X@l�@k�m@k�F@k��@kt�@kS�@ko@j��@j��@j�\@j�!@j~�@jM�@j=q@j-@j-@jJ@i��@ihs@h��@h�u@hQ�@h �@g�P@f�+@fV@fE�@f{@dZ@c�
@cC�@b�@b�@b�@b��@b��@b�!@b�\@bM�@a�#@aX@a�@`�`@`r�@` �@_�@_;d@^��@^�+@]�@]��@]�@]V@\�/@\�@\�@\Z@\9X@\1@[��@[��@[t�@[33@Z��@Z�\@Z�\@Z~�@Z-@Z�@ZJ@Y��@Y��@Yhs@YG�@Y7L@YG�@YX@Yx�@Y�7@Yx�@YX@Y7L@Y�@X��@XbN@XA�@W��@W\)@W;d@W;d@W;d@W+@W+@W+@W+@V�@Vff@U�@U?}@UV@Tz�@S��@S��@SdZ@So@R�H@R�!@R�!@R��@R�\@R~�@Rn�@RM�@R=q@R�@Q�@Q��@Qhs@P��@P��@Pr�@O�;@O|�@O+@Nff@N@M��@L��@L�/@L�j@Lz�@K��@K�@J�@J��@J��@J�\@JM�@JJ@I��@Ihs@H�`@HĜ@HĜ@H�u@HA�@G�@G\)@G+@G
=@F�@F��@Fff@E�T@Ep�@D��@D�D@D9X@C��@C�m@CdZ@B�@B�H@B��@B^5@BJ@A��@A�@A%@@��@@�`@@�@@�@@b@?��@?�@?�P@?|�@?\)@?;d@>�@>v�@=@<�@<1@;�F@;t�@;33@;@:��@:^5@:=q@:J@9��@9X@8��@81'@7�@7�@7�P@7
=@6�y@6�@6�@6�@6ȴ@6��@6�+@65?@5�T@5�@4��@4z�@4(�@3dZ@3@2��@2��@2��@2��@2^5@2�@1�@1��@1G�@0��@0�@0r�@0r�@0bN@0A�@/�w@/l�@/;d@/+@.�y@.��@.v�@.V@.5?@-�T@-��@-�-@-�h@-`B@-V@,�@,��@,��@,�j@,��@,Z@+��@+�F@+t�@+33@+33@*�H@*~�@)��@)��@)hs@)7L@(��@(�u@(1'@'�P@'
=@&�R@&��@&�+@&ff@&E�@%��@%p�@%?}@%/@$��@$�@$�@$j@$Z@$(�@$1@#��@#33@"��@"��@"~�@"n�@"=q@!�#@!��@!&�@ ��@ �u@ Q�@�@��@�P@|�@|�@|�@|�@+@��@v�@{@�T@��@��@@@�h@p�@`B@?}@V@�/@�j@�D@I�@(�@�@1@�
@o@�H@��@�!@n�@��@�@�@�#@��@x�@x�@�@bN@ �@�;@��@��@�P@l�@\)@K�@K�@K�@;d@�@�@ff@{@@@�T@@�@?}@V@�/@Z@��@�@t�@S�@"�@��@�\@n�@�@��@�^@��@�7@&�@�@��@Ĝ@�@1'@�@��@�P@�P@|�@�y@��@�+@v�@v�@ff@ff@E�@$�@@�h@p�@?}@��@z�@I�@1@��@t�@t�@S�@S�@S�@S�@S�@33@"�@
�!@
n�@
M�@	�@	��@	G�@	&�@	�@	�@	�@	&�@	�@	�@	%@	%@��@	%@��@��@Q�@ �@b@  @  @�@�;@�;@��@��@�w@��@��@�P@l�@�@�@��@��@��@ff@@�T@�T@�@�@�T@��@p�@��@�@��@I�@9X@�@1@��@��@�m@�m@�
@��@t�@o@��@��@�\@�\@n�@^5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A���A���A���A���A���A���A��
A��
A��
A��A��A��A��#A��#A��#A��#A��/A��/A��/A��/A��;A��;A��HA��HA��;A�ĜA�=qA�v�A��#A�7LAġ�AÃA�A���A���A��A�1'A�E�A��A�S�A��HA���A��A��FA�A���A��-A���A�A��A�VA�?}A��uA�n�A���A���A�v�A��A�M�A�-A�$�A�1'A��A��FA�(�A�p�A��mA�C�A�VA��
A���A�1'A�?}A�=qA���A�A�&�A��A��7A�7LA�JA�M�A�E�A��A�  A��HA�bAK�A|�AzȴAxA�AwdZAwC�Av�Au��At�uAsS�Ar~�Aq�#Aq�-Aql�Ap�Ao�AoAn��An�9An�+An=qAm�7Alv�Al{Akl�AjȴAj�Ai\)Ah�+AhbAg�;AgƨAg�FAg�-Ag��Af�Af�Ae�7Ae&�Adz�AcO�Abz�A`bA^�!A]��A]\)AZ�AY%AW��AV��AU�
ATARM�AQ�AO;dANȴAN��AM�mAMG�AKK�AHZAGx�AFQ�AD��AC��AB��AB��AB-AA�A@��A@��A@ffA?�#A?%A>-A>1A=��A=G�A<�\A;��A:�A9�A8�A8�`A8��A7"�A6I�A5��A4��A3�TA3�A2�RA0�HA0=qA/��A/�A.��A-�A, �A+A*ZA)��A)��A(�jA'��A&��A%��A%&�A$�9A#��A#A"�DA!��A!;dA!
=A �+Ap�A�A�A��AVA�-A�
AG�A&�A��A-A33A�A��AA�A/A��A�wA�A�HAȴA��AI�A�^AffA��A|�A
��A
�+A
A	|�A	�AI�A/AbNA{Ax�A�RAVA(�A��A
=A��A�\AffAQ�AE�A=qAbA��AVA bN@�@�S�@��\@�z�@�K�@�^5@�Ĝ@�7@�@�J@�j@���@�dZ@�-@�Ĝ@�"�@��/@��
@�C�@◍@��@�33@�@��`@ۍP@��`@�K�@֟�@Լj@҇+@�O�@�b@�+@θR@�M�@�p�@ʏ\@ɑh@�x�@Ɵ�@���@�z�@�(�@��@��@�\)@���@��7@�Z@�t�@�-@�{@�@���@�V@��/@��P@�hs@�|�@�&�@���@���@�@��-@��^@�@���@���@���@��-@�?}@���@��@�A�@���@�K�@�{@�hs@���@�1'@��P@�K�@��y@��-@�V@�j@�C�@���@���@��^@�`B@��@�  @��
@��@���@�dZ@�"�@���@�v�@�J@��7@���@�bN@�Q�@�A�@��@��m@�dZ@�"�@�@�ȴ@�~�@�5?@���@��h@�`B@�?}@��@��@���@��/@�A�@��F@�l�@�K�@�C�@�
=@��R@�-@�p�@���@��D@�1'@�1@��m@���@��P@�K�@�-@��T@��^@���@��7@��@��9@��9@���@���@��j@� �@���@��R@��#@��^@���@�7L@��@�z�@�(�@�  @�ƨ@��@�S�@��@�V@�=q@�=q@�-@��@���@��9@�bN@�  @�ƨ@�|�@�S�@�;d@���@��\@�v�@�E�@�@���@�G�@�&�@��`@���@��9@���@�z�@�Q�@�A�@�b@+@~�+@~@~@}�T@}�-@|�@{�m@{S�@{C�@{o@z�@z��@zJ@y�#@y��@y��@yX@x�`@xĜ@x�u@xbN@x1'@w��@w+@vȴ@v�+@v@u��@uV@t��@s�m@s��@s"�@r��@r=q@q�@q��@qhs@p��@p��@pr�@p �@pb@p  @p�@o�@o��@o;d@nv�@m�@l�@l9X@l�@k�m@k�F@k��@kt�@kS�@ko@j��@j��@j�\@j�!@j~�@jM�@j=q@j-@j-@jJ@i��@ihs@h��@h�u@hQ�@h �@g�P@f�+@fV@fE�@f{@dZ@c�
@cC�@b�@b�@b�@b��@b��@b�!@b�\@bM�@a�#@aX@a�@`�`@`r�@` �@_�@_;d@^��@^�+@]�@]��@]�@]V@\�/@\�@\�@\Z@\9X@\1@[��@[��@[t�@[33@Z��@Z�\@Z�\@Z~�@Z-@Z�@ZJ@Y��@Y��@Yhs@YG�@Y7L@YG�@YX@Yx�@Y�7@Yx�@YX@Y7L@Y�@X��@XbN@XA�@W��@W\)@W;d@W;d@W;d@W+@W+@W+@W+@V�@Vff@U�@U?}@UV@Tz�@S��@S��@SdZ@So@R�H@R�!@R�!@R��@R�\@R~�@Rn�@RM�@R=q@R�@Q�@Q��@Qhs@P��@P��@Pr�@O�;@O|�@O+@Nff@N@M��@L��@L�/@L�j@Lz�@K��@K�@J�@J��@J��@J�\@JM�@JJ@I��@Ihs@H�`@HĜ@HĜ@H�u@HA�@G�@G\)@G+@G
=@F�@F��@Fff@E�T@Ep�@D��@D�D@D9X@C��@C�m@CdZ@B�@B�H@B��@B^5@BJ@A��@A�@A%@@��@@�`@@�@@�@@b@?��@?�@?�P@?|�@?\)@?;d@>�@>v�@=@<�@<1@;�F@;t�@;33@;@:��@:^5@:=q@:J@9��@9X@8��@81'@7�@7�@7�P@7
=@6�y@6�@6�@6�@6ȴ@6��@6�+@65?@5�T@5�@4��@4z�@4(�@3dZ@3@2��@2��@2��@2��@2^5@2�@1�@1��@1G�@0��@0�@0r�@0r�@0bN@0A�@/�w@/l�@/;d@/+@.�y@.��@.v�@.V@.5?@-�T@-��@-�-@-�h@-`B@-V@,�@,��@,��@,�j@,��@,Z@+��@+�F@+t�@+33@+33@*�H@*~�@)��@)��@)hs@)7L@(��@(�u@(1'@'�P@'
=@&�R@&��@&�+@&ff@&E�@%��@%p�@%?}@%/@$��@$�@$�@$j@$Z@$(�@$1@#��@#33@"��@"��@"~�@"n�@"=q@!�#@!��@!&�@ ��@ �u@ Q�@�@��@�P@|�@|�@|�@|�@+@��@v�@{@�T@��@��@@@�h@p�@`B@?}@V@�/@�j@�D@I�@(�@�@1@�
@o@�H@��@�!@n�@��@�@�@�#@��@x�@x�@�@bN@ �@�;@��@��@�P@l�@\)@K�@K�@K�@;d@�@�@ff@{@@@�T@@�@?}@V@�/@Z@��@�@t�@S�@"�@��@�\@n�@�@��@�^@��@�7@&�@�@��@Ĝ@�@1'@�@��@�P@�P@|�@�y@��@�+@v�@v�@ff@ff@E�@$�@@�h@p�@?}@��@z�@I�@1@��@t�@t�@S�@S�@S�@S�@S�@33@"�@
�!@
n�@
M�@	�@	��@	G�@	&�@	�@	�@	�@	&�@	�@	�@	%@	%@��@	%@��@��@Q�@ �@b@  @  @�@�;@�;@��@��@�w@��@��@�P@l�@�@�@��@��@��@ff@@�T@�T@�@�@�T@��@p�@��@�@��@I�@9X@�@1@��@��@�m@�m@�
@��@t�@o@��@��@�\@�\@n�@^5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�#B�B�B�B�B�B�B��B��B��B  B��B��B��B�B�fB�TB�;B��B�3B��B��B�oB�hB�1B~�Br�BgmBYBT�BN�BH�B49B�BPB
=BB�`B��BÖB�wB�?B��B�hB�1Bv�BiyBW
BD�B;dB49B0!B+B�B
��B
�B
�TB
�B
ÖB
�3B
��B
�uB
�7B
w�B
o�B
n�B
k�B
cTB
W
B
L�B
F�B
@�B
>wB
=qB
9XB
1'B
1'B
1'B
1'B
0!B
.B
,B
#�B
 �B
�B
�B
�B
hB
	7B
%B
B
B
B
B
B	��B	��B	�B	�B	�B	�`B	�BB	��B	ƨB	��B	�^B	�B	��B	��B	�\B	�1B	y�B	l�B	aHB	W
B	R�B	Q�B	M�B	I�B	A�B	2-B	)�B	$�B	�B	�B	oB	hB	VB		7B	
=B	
=B		7B	1B	1B	B	B	B	B��B��B��B�B�B�B�B�sB�TB�BB�)B�)B�
B�B��B��B��BɺBȴB��B�jB�RB�FB�3B�-B�B�B��B��B��B��B��B��B��B�{B�hB�bB�\B�DB�7B�%B�B�B~�B{�Bx�Bw�Bw�Bv�Bs�Bo�Bk�Be`BaHB_;B\)BZBYBYBXBW
BVBQ�BO�BN�BL�BK�BJ�BH�BG�BD�BB�B>wB=qB=qB;dB:^B9XB9XB6FB5?B49B49B49B49B33B33B2-B1'B.B/B+B(�B'�B%�B#�B#�B�B�B�B�B�B�B�B�B�B�B{BuBoBuBhBbBbBhB\BVBVBVB\BPBVBPBPBPBVBuBhBbB�B�B�B�B�B�B�B�B �B#�B(�B+B+B+B.B.B-B.B+B,B0!B/B2-B7LB8RB:^B;dB;dB;dB<jB=qBB�BK�BM�BP�BQ�BR�BW
BYBZB\)B]/B^5B_;BbNBcTBe`BiyBjBl�Bn�Bn�Bp�Bq�Br�Br�Bt�Bu�Bv�Bw�Bx�By�Bz�B}�B�B�B�B�%B�+B�DB�DB�JB�JB�PB�\B�bB�oB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�-B�9B�^B�qB�wB�wB�}BBŢBŢBƨBȴB��B��B��B�B�BB�HB�NB�fB�mB�B�B�B�B��B��B��B��B��B��B	  B	B	B	DB	\B	{B	�B	�B	�B	�B	�B	 �B	!�B	#�B	%�B	)�B	-B	.B	1'B	2-B	33B	49B	6FB	7LB	8RB	:^B	?}B	B�B	E�B	E�B	E�B	F�B	I�B	N�B	O�B	P�B	Q�B	R�B	S�B	W
B	XB	YB	ZB	[#B	]/B	^5B	_;B	`BB	aHB	cTB	dZB	dZB	e`B	gmB	hsB	jB	l�B	o�B	r�B	t�B	t�B	u�B	w�B	w�B	{�B	|�B	� B	�B	�B	�+B	�=B	�\B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�3B	�9B	�?B	�9B	�9B	�9B	�?B	�?B	�?B	�LB	�RB	�XB	�dB	�wB	�wB	�}B	�}B	��B	ÖB	ÖB	ÖB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�/B	�5B	�BB	�NB	�NB	�TB	�ZB	�`B	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
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
1B
	7B

=B

=B

=B

=B
DB
JB
PB
VB
\B
\B
\B
bB
hB
hB
oB
oB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
#�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
-B
.B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
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
>wB
>wB
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
K�B
L�B
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
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
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
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
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
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
m�B
m�B
m�B
m�B
m�B
n�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B� B��B��B��B�2B�B�B��B�5B�[B�B�`B��B�6B��B�B �B��B�LB��B�
B�2B�BؓB��B��B�=B�MB��B��B�Bv`Bi_BZBV�BQ�BMjB8RB�B�BBEB��B̈́BňB�B�lB�B��B��By�BlqBY�BFB<�B5B1�B./BdBoB
�=B
�B
��B
�%B
��B
��B
�B
��B
x�B
p!B
o�B
mB
d�B
X�B
M�B
GzB
@�B
?B
>]B
:�B
1�B
1vB
1vB
1�B
0�B
/5B
-CB
$�B
!�B
�B
eB
�B
oB
	�B
tB
SB
MB
GB
�B
B
  B	��B	�B	��B	�5B	�B	��B	ӏB	��B	�B	�<B	�OB	�BB	�B	�4B	��B	|B	ncB	cTB	W�B	S�B	S&B	O\B	L~B	D�B	3�B	+�B	&�B	!-B	eB	B	oB	�B		�B	
�B	
�B	
#B		lB		7B	�B	�B	-B	'B�(B�jB��B�'B� B�B�iB�B�B�bBݲB�IB�EB�B��B̈́B�~B�DB��B�B��B�XB��B�B��B��B��B��B��B��B��B�B�B��B�MB�B�hB��B�JB�rB�EB��B�aB� B|�ByXBx�Bx�BxlBu�BrBnIBf�BbNB`vB]BZ�BY�BY�BX�BX+BW�BR�BP�BO�BM�BL�BK�BI�BIBF%BC�B?.B>wB>wB<B:�B:*B:^B6�B5�B4�B4�B4�B4�B3�B4B3hB2|B0;B0�B,B*eB(�B&�B%zB%�B!bB�B�BCBQB�BB�B�B9B2BFB�BaBB�B�BB}B(B�B�BHB<B�B�BBVBB{B:B�B_B�B_BEBBdB~B�B!�B$�B)�B+kB+kB+�B.�B.�B.cB/�B,�B-�B0�B0;B3hB7�B8�B:�B;�B;�B;�B<�B>BB�BL0BNpBQ�BR�BS�BW�BY�BZ�B\�B]�B^�B`'Bb�BdBf2Bi�BkBm]BoBo5BqABq�BsBr�BuBvFBw2BxRByXBz�B{�B~]B�GB�gB�mB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�9B�B�B��B��B�-B�hB��B�zB�mB�XB�yB�QB�QB�WB��B��B�B��B��B��B��B� B��BżBżB��B�B�^B�vB��BںB�B�B��B��B��B��B��B��B�B�%B�FB�XB�(B�.B�cB	 iB	�B	�B	�B	�B	�B	�B	�B	�B	)B	�B	!B	"B	$@B	&LB	*KB	-]B	.cB	1vB	2aB	3hB	4�B	6zB	7�B	8�B	:�B	?�B	B�B	E�B	E�B	E�B	GB	J=B	O(B	PB	QB	R B	S@B	TFB	W?B	X_B	YKB	ZkB	[qB	]dB	^�B	_pB	`�B	a�B	c�B	d�B	d�B	e�B	g�B	h�B	j�B	l�B	o�B	r�B	uB	u%B	u�B	xB	x8B	|6B	}VB	�4B	�;B	�AB	�EB	�XB	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�KB	�QB	�=B	�]B	�cB	�iB	�[B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�lB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�"B	�B	�.B	�4B	�B	�B	�:B	�@B	�FB	�2B	�9B	�?B	�_B	�KB	چB	�IB	�OB	�vB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	�2B	�	B	�B	�B	��B	�B	��B	�B	�B	�JB	�6B	�qB	�.B
 OB
 iB
uB
GB
MB
mB
SB
SB
YB
?B
YB
?B
YB
tB
_B
zB
_B
_B
�B
�B
	lB

�B

�B

�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
�B
�B
B
B
�B
�B
 B
 B
!B
!-B
!�B
!�B
"B
#B
$&B
#:B
$B
$B
%B
%B
%,B
%,B
%,B
&LB
&LB
'�B
)DB
)*B
*0B
*0B
*KB
+6B
+QB
+6B
+6B
,=B
,qB
-]B
.cB
.IB
.IB
/OB
/iB
0UB
0;B
0;B
0;B
0oB
0UB
0UB
0oB
0oB
1vB
1vB
2aB
3�B
3�B
4nB
4nB
5ZB
5�B
5tB
5tB
5tB
6zB
6zB
6�B
7�B
7�B
8lB
8lB
8�B
8�B
8�B
8�B
9�B
9�B
9�B
9�B
:�B
:�B
:�B
:�B
;�B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
IB
I�B
J	B
J	B
J�B
K�B
LB
K�B
LB
L�B
L�B
L�B
MB
MB
MB
M6B
N"B
NB
OB
OB
N�B
N�B
OB
OB
OB
O�B
PB
PB
PB
PB
QB
QB
Q4B
QB
QB
R:B
RTB
S&B
S&B
S&B
S@B
S&B
TB
UB
U2B
UMB
U2B
UMB
UMB
VSB
W?B
WYB
X+B
XEB
XEB
XEB
X+B
XEB
XEB
X+B
XEB
X_B
XEB
YeB
YKB
YKB
Y1B
YKB
YKB
YeB
YKB
YKB
YKB
Z�B
ZkB
[=B
[qB
[qB
[WB
\xB
\]B
\]B
\]B
]dB
]IB
]dB
]dB
]dB
^OB
^�B
^jB
^jB
_pB
_pB
_�B
`\B
`\B
`vB
`�B
`vB
`\B
`\B
`vB
abB
abB
a�B
a|B
a|B
a�B
a|B
b�B
b�B
c�B
c�B
c�B
c�B
d�B
dtB
dtB
dtB
dtB
dtB
dtB
d�B
d�B
d�B
e�B
e�B
e�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
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
l�B
m�B
m�B
n�B
m�B
m�B
m�B
m�B
m�B
n�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o B
o�B
o�B
o�B
o�B
o�B
o�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<AV�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.25(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201611030037372016110300373720161103003737201806221216062018062212160620180622121606201804050408592018040504085920180405040859  JA  ARFMdecpA19c                                                                20161030093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161030003518  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161030003518  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161030003519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161030003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161030003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161030003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161030003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161030003520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161030003520                      G�O�G�O�G�O�                JA  ARUP                                                                        20161030013249                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161030153317  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20161102153737  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161102153737  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190859  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031606  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111518                      G�O�G�O�G�O�                