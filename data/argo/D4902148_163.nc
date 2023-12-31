CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-02-02T19:18:53Z creation;2019-02-02T19:18:57Z conversion to V3.1;2019-12-18T07:17:14Z update;2022-11-21T05:29:27Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        l  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  Mh   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ̨   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190202191853  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_163                     2C  Dd�NAVIS_A                         0397                            ARGO 011514                     863 @ؤ��"�1   @ؤ��/h�@< ѷY�d����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D	  D	y�D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D¼�D�  D�@ DÀ D�� D�  D�@ D�|�D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @)��@p  @�  @�  A  A<  A\  A|  A�  A�  A�  A�  A���A�  A�  A�  B  B  B  B  B'  B/  B7  B?  BGffBO  BW  B_  Bg  Bo  Bw  B  B�� B�� B�� B�� B�L�B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BÀ Bǀ Bˀ Bπ BӀ B׀ Bۀ B߀ B� B� B� B� B� B�� B�� B�� C� C� C� C� C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI�fCK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� CyٚC{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D p D � Dp D� Dp D� Dp D� Dp D� Dp D� Di�D� Dp D� Dp D� D	i�D	� D
p D
� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� DvfD� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D p D � D!p D!� D"p D"� D#p D#� D$p D$� D%p D%� D&p D&� D'p D'� D(p D(� D)p D)� D*p D*� D+p D+� D,p D,� D-p D-� D.p D.� D/p D/� D0p D0� D1p D1� D2p D2� D3p D3� D4p D4� D5p D5� D6p D6� D7p D7� D8p D8� D9p D9� D:p D:� D;p D;� D<p D<� D=p D=� D>p D>� D?p D?� D@p D@� DAp DA� DBp DB� DCp DC� DDp DD� DEp DE� DFp DF� DGp DG� DHp DH� DIp DI� DJp DJ� DKp DK� DLp DL� DMp DM� DNp DN� DOp DO� DPp DP� DQp DQ� DRp DR� DSp DS� DTp DT� DUp DU� DVp DV� DWp DW� DXp DX� DYp DY� DZp DZ� D[p D[� D\p D\� D]p D]� D^p D^� D_p D_� D`p D`� Dap Da� Dbp Db� Dcp Dc� Ddp Dd� Dep De� Dfp Df� Dgp Dg� Dhp Dh� Dip Di� Djp Dj� Dkp Dk� Dlp Dl� Dmp Dm� Dnp Dn� Dop Do� Dpp Dp� Dqp Dq� Drp Dr� Dsp Ds� Dtp Dt� Dup Du� Dvp Dv� Dwp Dw� Dxp Dx� Dyp Dy� Dzp Dz� D{p D{� D|p D|� D}p D}� D~p D~� Dp D� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�4�D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D´�D�� D�8 D�x Dø D�� D�8 D�t�Dĸ D�� D�8 D�x DŸ D�� D�8 D�x DƸ D�� D�8 D�x DǸ D�� D�8 D�x Dȸ D�� D�8 D�x Dɸ D�� D�8 D�x Dʸ D�� D�8 D�x D˸ D�� D�8 D�x D̸ D�� D�8 D�x D͸ D�� D�8 D�x Dθ D�� D�8 D�x Dϸ D�� D�8 D�x Dи D�� D�8 D�x DѸ D�� D�8 D�x DҸ D�� D�8 D�x DӸ D�� D�8 D�x DԸ D�� D�8 D�x Dո D�� D�8 D�x Dָ D�� D�8 D�x D׸ D�� D�8 D�x Dظ D�� D�8 D�x Dٸ D�� D�8 D�x Dڸ D�� D�8 D�x D۸ D�� D�8 D�x Dܸ D�� D�8 D�x Dݸ D�� D�8 D�x D޸ D�� D�8 D�x D߸ D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D�� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111FA���A���A���A���A���A���A��+A��7A��uA��7A��+A��uA���A���A���A���A���A���A���A���A��A��A��A��A���A��A��A��A��A��!A��-A��9A��FA��9A��9A���A��A�C�A�jA��TA��A�JA�VA���A�A�ĜA��A��\A�~�A��9A�r�A��yA�^5A���A���A�7LA�S�A��HA���A��uA��#A���A��A��A��A�x�A�A���A�oA�VA��+A�33A�jA�M�A��;A�S�A}33A{x�Az�AyO�Ax��Aw��Av�Au��AuAt�9AtZAsƨAs�Ar�!Ap�Ao��An��AnffAjVAh��AhA�AhJAg��Ag��Ag��Ag�^Ag�-Ag��AghsAgoAf�`Ae�Aa\)A`5?A_+A]�
A]+A\�/A\ffA[��A[�AZr�AZjAZI�AY33AVI�ATȴAS��AS7LAS"�AR��AR��AR��AR^5AP��AN�AN�AL��AL��AKdZAJ�/AJ  AI�7AH�AH=qAH1AH  AG�;AG�AF�AE��AE;dAD�ADȴADjAB��ABZAA|�AAdZAAG�A@��A@  A?|�A>�9A=��A=�7A<~�A;�A:ĜA:Q�A9�mA9��A9/A9%A85?A7�hA6VA4�A2�yA1�^A0�A0~�A/ƨA/�A.��A.VA.�A-�A-&�A,��A,r�A,�A+ƨA*�A*I�A*  A(��A(ZA'hsA&E�A%dZA$��A#�mA#��A#�^A#��A#�A"��A"VA"(�A!�A!dZA!XA!%A �!A �uA 1'A�;A��A�PA�DA�A��A&�A��AAVAz�A��A�AA�A\)AVA��A|�A��AA�A�-A�AXA
�9A	�
A��A?}A�mA��A�/AA;dAA�+A �@��H@��@�Ĝ@�A�@��@�V@��T@���@�J@��T@�h@�O�@�1@�S�@�o@�@�@�A�@�t�@��@��T@��/@�1'@畁@���@��@��@�bN@�M�@��#@�`B@�&�@��/@�;d@�Ĝ@��@֗�@�n�@ԃ@�K�@�=q@Ѻ^@��@�  @�"�@��H@�v�@�@�%@̋D@�(�@���@��
@���@őh@���@�1@�S�@��@��@�
=@��y@\@��#@�`B@�r�@��^@��@�\)@��@�r�@���@��@�K�@��@�-@��`@��P@�C�@��\@�`B@�G�@�/@�%@�Ĝ@�  @�33@�5?@���@��F@��@�n�@�-@��@�%@��u@�Q�@�9X@��F@�\)@���@�ff@�{@�/@��F@�33@���@�5?@���@���@��@�X@�(�@��@��@��+@�=q@�?}@�I�@� �@��w@�\)@�+@�
=@���@�^5@�J@���@�`B@�bN@�(�@��@��m@��w@�o@��R@��!@�~�@�V@�5?@��@�{@��@���@�`B@�G�@�?}@��@��`@���@�z�@��@�K�@�+@�
=@���@��@���@�$�@���@�`B@��`@�Q�@�9X@�1'@�  @��P@��@��@��@�33@��@�-@���@�?}@��`@���@�r�@�A�@�(�@�(�@��@�1@�33@��@��R@�V@��T@���@�x�@��@���@��D@�Z@�b@�|�@��H@��!@�n�@�V@�5?@��@�7L@��@��j@���@��u@��@�r�@�bN@�I�@�9X@�(�@l�@~��@~�@~��@~�+@~V@~5?@~@}��@}�@}V@|�/@|��@|�D@{�m@{o@yx�@y�@y%@xr�@x  @w�P@w+@v�+@u�T@u�-@u�h@u�@t�@t��@tj@s�m@s��@s��@s33@r��@r=q@q�#@p�@pb@p  @o��@o\)@o+@o
=@n�y@n��@m�T@mO�@l�/@l�D@k�m@kt�@kC�@j~�@h��@hQ�@h  @g��@g��@g��@g��@g��@g�P@gK�@g
=@f�y@f�@fȴ@fv�@fff@e�@d��@dZ@d9X@d(�@d�@c�m@c��@b�@b~�@b=q@b-@a�^@`��@_�P@]��@]/@\��@\��@\��@\Z@\9X@\(�@\(�@\(�@\(�@\(�@[��@[�m@[�F@[��@[t�@[C�@["�@[o@Z�H@Z��@Z�\@Z~�@Zn�@Y��@Yhs@Y�@XĜ@XQ�@W\)@W+@W�@W�@W�@W
=@V�y@V�@Vv�@U�@U�-@U�-@U�@U�@U�@UV@T�@S��@S�F@SS�@Rn�@Qhs@P�`@Pb@O�P@O�@N��@N��@N��@N�y@N��@N�+@N�+@Nv�@NV@NE�@N{@N@M�@M@Mp�@MO�@L��@LZ@L9X@L(�@K��@K�
@Kt�@KC�@Ko@Ko@J��@Jn�@JJ@I�@I��@I��@I��@IG�@HĜ@H  @G�w@G|�@G\)@G+@G
=@F�@F�R@F�R@F�R@F�R@F$�@F@E�@E�h@E?}@D�@CdZ@B�H@BM�@BJ@A�@A�#@A�#@A��@A�^@A�^@Ax�@AG�@A�@A�@@��@@1'@?�@?l�@?;d@?
=@>�@>ff@>@=�-@=O�@<Z@;"�@:�\@:M�@:-@:J@9��@9��@9�7@9x�@9X@9&�@8��@8bN@8A�@8b@7�;@7��@7�@6�+@6V@65?@5@5O�@5V@4��@4I�@3�
@3�@3C�@333@3o@2�@2�!@2��@2�\@2~�@2M�@1�7@0�`@0�@0bN@0 �@0  @/�@/�@/\)@/;d@/
=@.�@.ȴ@.��@.v�@.$�@-@-��@-��@-��@-��@-�h@-��@-��@-�@-?}@,��@,�j@,��@,�D@,I�@,�@+�m@+�
@+ƨ@+��@+o@*�H@*~�@*-@)�#@)��@)7L@)%@(Ĝ@(1'@'�@'�;@'�@'��@'l�@'
=@&��@&v�@&$�@%��@%�h@%p�@%?}@$�@$�D@$j@$I�@$�@#ƨ@#��@#��@#dZ@#@"�\@"-@!X@ ��@ Ĝ@ �9@ �9@ �u@ bN@ Q�@ b@   @�w@��@K�@��@��@V@@�h@O�@�@�@�@�@�@V@V@��@�@��@�@��@Z@1@1@1@1@1@�@�@1@ƨ@t�@��@�@J@J@��@hs@7L@�`@��@�@r�@�;@|�@;d@+@
=@�y@ȴ@��@E�@@�T@��@@@@O�@��@�@�@(�@1@1@��@�m@�F@��@t�@dZ@@�\@��@�#@hs@��@�u@bN@Q�@A�@1'@b@  @�@|�@��@v�@ff@V@5?@{@�@�-@�@`B@?}@/@�@��@�@��@z�@j@9X@�
@��@�@�@t�@S�@@
��@
~�@
M�@
-@
�@	�@	��@	��@	��@	�7@	x�@	x�@	hs@	hs@	hs@	hs@	hs@	hs@	hs@	G�@	�@�@�;@�@�P@�P@�P@l�@l�@\)@+@�y@�R@v�@5?@$�@@�T@��@�@�@��@�/@��@�j@�j@�@�@�@�@��@�D@j@�@�
@S�@��@��@~�@=q@J@��@�@�#@�#@��@��@��@�7@x�@X@&�@ ��@ Ĝ@ ��@ ��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111FA���A���A���A���A���A���A��+A��7A��uA��7A��+A��uA���A���A���A���A���A���A���A���A��A��A��A��A���A��A��A��A��A��!A��-A��9A��FA��9A��9A���A��A�C�A�jA��TA��A�JA�VA���A�A�ĜA��A��\A�~�A��9A�r�A��yA�^5A���A���A�7LA�S�A��HA���A��uA��#A���A��A��A��A�x�A�A���A�oA�VA��+A�33A�jA�M�A��;A�S�A}33A{x�Az�AyO�Ax��Aw��Av�Au��AuAt�9AtZAsƨAs�Ar�!Ap�Ao��An��AnffAjVAh��AhA�AhJAg��Ag��Ag��Ag�^Ag�-Ag��AghsAgoAf�`Ae�Aa\)A`5?A_+A]�
A]+A\�/A\ffA[��A[�AZr�AZjAZI�AY33AVI�ATȴAS��AS7LAS"�AR��AR��AR��AR^5AP��AN�AN�AL��AL��AKdZAJ�/AJ  AI�7AH�AH=qAH1AH  AG�;AG�AF�AE��AE;dAD�ADȴADjAB��ABZAA|�AAdZAAG�A@��A@  A?|�A>�9A=��A=�7A<~�A;�A:ĜA:Q�A9�mA9��A9/A9%A85?A7�hA6VA4�A2�yA1�^A0�A0~�A/ƨA/�A.��A.VA.�A-�A-&�A,��A,r�A,�A+ƨA*�A*I�A*  A(��A(ZA'hsA&E�A%dZA$��A#�mA#��A#�^A#��A#�A"��A"VA"(�A!�A!dZA!XA!%A �!A �uA 1'A�;A��A�PA�DA�A��A&�A��AAVAz�A��A�AA�A\)AVA��A|�A��AA�A�-A�AXA
�9A	�
A��A?}A�mA��A�/AA;dAA�+A �@��H@��@�Ĝ@�A�@��@�V@��T@���@�J@��T@�h@�O�@�1@�S�@�o@�@�@�A�@�t�@��@��T@��/@�1'@畁@���@��@��@�bN@�M�@��#@�`B@�&�@��/@�;d@�Ĝ@��@֗�@�n�@ԃ@�K�@�=q@Ѻ^@��@�  @�"�@��H@�v�@�@�%@̋D@�(�@���@��
@���@őh@���@�1@�S�@��@��@�
=@��y@\@��#@�`B@�r�@��^@��@�\)@��@�r�@���@��@�K�@��@�-@��`@��P@�C�@��\@�`B@�G�@�/@�%@�Ĝ@�  @�33@�5?@���@��F@��@�n�@�-@��@�%@��u@�Q�@�9X@��F@�\)@���@�ff@�{@�/@��F@�33@���@�5?@���@���@��@�X@�(�@��@��@��+@�=q@�?}@�I�@� �@��w@�\)@�+@�
=@���@�^5@�J@���@�`B@�bN@�(�@��@��m@��w@�o@��R@��!@�~�@�V@�5?@��@�{@��@���@�`B@�G�@�?}@��@��`@���@�z�@��@�K�@�+@�
=@���@��@���@�$�@���@�`B@��`@�Q�@�9X@�1'@�  @��P@��@��@��@�33@��@�-@���@�?}@��`@���@�r�@�A�@�(�@�(�@��@�1@�33@��@��R@�V@��T@���@�x�@��@���@��D@�Z@�b@�|�@��H@��!@�n�@�V@�5?@��@�7L@��@��j@���@��u@��@�r�@�bN@�I�@�9X@�(�@l�@~��@~�@~��@~�+@~V@~5?@~@}��@}�@}V@|�/@|��@|�D@{�m@{o@yx�@y�@y%@xr�@x  @w�P@w+@v�+@u�T@u�-@u�h@u�@t�@t��@tj@s�m@s��@s��@s33@r��@r=q@q�#@p�@pb@p  @o��@o\)@o+@o
=@n�y@n��@m�T@mO�@l�/@l�D@k�m@kt�@kC�@j~�@h��@hQ�@h  @g��@g��@g��@g��@g��@g�P@gK�@g
=@f�y@f�@fȴ@fv�@fff@e�@d��@dZ@d9X@d(�@d�@c�m@c��@b�@b~�@b=q@b-@a�^@`��@_�P@]��@]/@\��@\��@\��@\Z@\9X@\(�@\(�@\(�@\(�@\(�@[��@[�m@[�F@[��@[t�@[C�@["�@[o@Z�H@Z��@Z�\@Z~�@Zn�@Y��@Yhs@Y�@XĜ@XQ�@W\)@W+@W�@W�@W�@W
=@V�y@V�@Vv�@U�@U�-@U�-@U�@U�@U�@UV@T�@S��@S�F@SS�@Rn�@Qhs@P�`@Pb@O�P@O�@N��@N��@N��@N�y@N��@N�+@N�+@Nv�@NV@NE�@N{@N@M�@M@Mp�@MO�@L��@LZ@L9X@L(�@K��@K�
@Kt�@KC�@Ko@Ko@J��@Jn�@JJ@I�@I��@I��@I��@IG�@HĜ@H  @G�w@G|�@G\)@G+@G
=@F�@F�R@F�R@F�R@F�R@F$�@F@E�@E�h@E?}@D�@CdZ@B�H@BM�@BJ@A�@A�#@A�#@A��@A�^@A�^@Ax�@AG�@A�@A�@@��@@1'@?�@?l�@?;d@?
=@>�@>ff@>@=�-@=O�@<Z@;"�@:�\@:M�@:-@:J@9��@9��@9�7@9x�@9X@9&�@8��@8bN@8A�@8b@7�;@7��@7�@6�+@6V@65?@5@5O�@5V@4��@4I�@3�
@3�@3C�@333@3o@2�@2�!@2��@2�\@2~�@2M�@1�7@0�`@0�@0bN@0 �@0  @/�@/�@/\)@/;d@/
=@.�@.ȴ@.��@.v�@.$�@-@-��@-��@-��@-��@-�h@-��@-��@-�@-?}@,��@,�j@,��@,�D@,I�@,�@+�m@+�
@+ƨ@+��@+o@*�H@*~�@*-@)�#@)��@)7L@)%@(Ĝ@(1'@'�@'�;@'�@'��@'l�@'
=@&��@&v�@&$�@%��@%�h@%p�@%?}@$�@$�D@$j@$I�@$�@#ƨ@#��@#��@#dZ@#@"�\@"-@!X@ ��@ Ĝ@ �9@ �9@ �u@ bN@ Q�@ b@   @�w@��@K�@��@��@V@@�h@O�@�@�@�@�@�@V@V@��@�@��@�@��@Z@1@1@1@1@1@�@�@1@ƨ@t�@��@�@J@J@��@hs@7L@�`@��@�@r�@�;@|�@;d@+@
=@�y@ȴ@��@E�@@�T@��@@@@O�@��@�@�@(�@1@1@��@�m@�F@��@t�@dZ@@�\@��@�#@hs@��@�u@bN@Q�@A�@1'@b@  @�@|�@��@v�@ff@V@5?@{@�@�-@�@`B@?}@/@�@��@�@��@z�@j@9X@�
@��@�@�@t�@S�@@
��@
~�@
M�@
-@
�@	�@	��@	��@	��@	�7@	x�@	x�@	hs@	hs@	hs@	hs@	hs@	hs@	hs@	G�@	�@�@�;@�@�P@�P@�P@l�@l�@\)@+@�y@�R@v�@5?@$�@@�T@��@�@�@��@�/@��@�j@�j@�@�@�@�@��@�D@j@�@�
@S�@��@��@~�@=q@J@��@�@�#@�#@��@��@��@�7@x�@X@&�@ ��@ Ĝ@ ��@ ��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�BŢBŢBŢBŢBŢBŢBƨBƨBƨBƨBƨBƨBŢBŢBŢBŢBŢBŢBŢBƨBŢBŢBŢBŢBŢBŢBŢBŢBŢBƨBƨBŢBŢBĜBB�Bv�BF�B<jB6FB.B&�B�B�B�BoBB�5BɺB�wB�B��B��B��B��B�\B�Bk�BM�B=qB2-B.B&�B �B�B+B
�B
�/B
�B
ǮB
��B
�^B
�B
��B
��B
�\B
y�B
l�B
ffB
^5B
YB
S�B
K�B
F�B
B�B
?}B
<jB
9XB
6FB
0!B
$�B
�B
�B
hB	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�fB	�TB	�B	B	�jB	�LB	�'B	�B	�B	��B	��B	��B	��B	��B	��B	�uB	�B	{�B	u�B	s�B	r�B	q�B	p�B	o�B	k�B	dZB	YB	T�B	O�B	L�B	G�B	C�B	?}B	=qB	9XB	6FB	49B	49B	2-B	-B	'�B	%�B	#�B	!�B	 �B	�B	�B	uB	bB	\B	VB	DB	+B	B	  B��B��B��B�B�B�B�yB�mB�fB�ZB�NB�5B�
B��BǮBÖB��B��B�wB�jB�^B�XB�RB�FB�?B�3B�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�oB�hB�hB�bB�\B�VB�VB�PB�DB�+B�B}�Bz�Bv�Br�Bp�Bo�Bo�Bn�Bl�BiyBffBbNB\)BVBT�BS�BQ�BP�BO�BN�BL�BH�BD�BA�B>wB<jB9XB8RB7LB6FB49B1'B/B.B-B,B+B'�B&�B%�B%�B$�B$�B#�B#�B#�B#�B"�B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B#�B&�B+B-B.B.B-B,B,B0!B/B33B49B49B6FB9XB;dB>wBB�BA�BA�BC�BD�BD�BD�BD�BG�BI�BJ�BK�BP�BR�BT�BVBXBYB[#B]/B_;BaHBcTBiyBo�Bo�Br�By�Bz�B}�B�B�B�+B�+B�+B�\B�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�LB�RB�XB�^B�dB�}BBBÖBĜBŢBŢBŢBƨBȴB��B��B��B��B��B��B��B�B�B�B�B�#B�#B�#B�BB�NB�`B�sB�B�B�B�B�B�B�B�B��B��B��B	  B	B	B	+B	1B	
=B	
=B	
=B	DB	DB	bB	oB	{B	�B	�B	�B	�B	!�B	#�B	%�B	'�B	)�B	.B	33B	49B	6FB	7LB	8RB	9XB	>wB	?}B	A�B	B�B	C�B	C�B	D�B	E�B	E�B	F�B	G�B	J�B	L�B	M�B	N�B	O�B	P�B	P�B	Q�B	R�B	S�B	VB	W
B	W
B	XB	ZB	\)B	cTB	dZB	dZB	ffB	hsB	iyB	k�B	l�B	o�B	p�B	r�B	t�B	x�B	z�B	|�B	}�B	}�B	~�B	� B	�B	�B	�B	�+B	�7B	�7B	�=B	�DB	�DB	�DB	�JB	�JB	�PB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�-B	�3B	�9B	�9B	�?B	�RB	�jB	��B	ÖB	ĜB	ŢB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�
B	�
B	�
B	�
B	�B	�B	�#B	�)B	�5B	�;B	�;B	�;B	�HB	�TB	�TB	�TB	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
	7B
JB
PB
\B
\B
bB
bB
bB
bB
bB
bB
hB
hB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
'�B
'�B
(�B
)�B
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
/B
/B
/B
/B
0!B
1'B
2-B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
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
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
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
T�B
VB
VB
VB
VB
VB
W
B
W
B
XB
XB
W
B
XB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
cTB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
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
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
p�B
o�B
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
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�BżBżBżBżB��B��B��B��B��B��BƨBƨBżBżBżBżBżBżBżB��BżBżBżBżBżBżBżBżBżB��B��B��B�%B�%B�+B�lB}�BH�B>B8�B0UB)_B!BB�B�B�B��B��B�B�}B�&B��B�sB�$B�B��Bo�BP�B?B3B/�B(sB#BjB
�B
��B
�!B
�+B
�B
��B
��B
��B
�HB
��B
��B
|B
m�B
g�B
_VB
ZQB
U�B
L�B
GzB
C-B
@B
=<B
:B
7�B
2-B
&fB
!B
eB
gB	��B	�MB	�B	��B	��B	��B	��B	��B	��B	�B	�0B	�B	�B	��B	�MB	��B	��B	�B	��B	��B	��B	��B	��B	�B	�~B	��B	��B	�B	}<B	v`B	tB	sB	rB	q'B	p�B	m�B	f�B	ZkB	VSB	P�B	NpB	H�B	D�B	@OB	>]B	:*B	6�B	4�B	4�B	3MB	.IB	(�B	&�B	$ZB	"hB	!�B	�B	_B	{B	�B	�B	\B	JB	B	9B	 B��B�jB�LB�;B�cB�=B�B�$B�B�B�B�B�1B�[B�RBĶB�[B��B�cB�"B��B��B�	B�2B��B��B��B�B�UB�B��B�yB��B�:B�;B��B��B��B��B��B��B�
B��B�,B��B�@B��B��B� B��B��B�B�<B��B�B�{B.B|�Bx�Bs�BrBp�Bp�Bo�Bm�Bk�BhsBezB_BW�BVBT�BR�BQhBP}BPBNVBJ�BF�BC-B?HB=�B:�B9>B8B7�B6zB33B0UB.}B-�B,�B,�B*�B(�B&�B&2B%`B%`B$�B$tB$@B$tB#�B#B"�B!|B �B�BjBOB~B~B�B�BB5BIBOB~BIB�B�BVBpB!B�B�BVB�B�BpB!B;BpBpBpB�BpBB�B�BdB�BOBB 'B$&B'mB+�B-�B.�B/OB/5B.IB./B0�B0�B3�B4�B4�B6�B:DB<�B?cBCBBABB[BC�BEBEBEBE�BH�BJ�BK�BL�BQ�BSuBU�BV�BX�BY�B[�B]~B_�Ba�Bd&Bi�Bp!Bp�Bs�Bz^B{dB~�B�{B�mB�zB��B�KB��B�B�B�+B�B�pB� B�:B�@B�2B�RB�XB�eB��B�}B��B��B��B��B��B��B�B��B��B��B��B��B��B��B�B�+B�B��B��B�0B�6B�BB�bBуB�mB�eB�QB�kB�WBیB��B��B��B��B��B��B��B��B�B��B��B��B�B�%B��B�jB	 iB	uB	�B	�B	�B	
rB	
rB	
rB	�B	B	�B	�B	�B	B	�B	B	!B	"B	$&B	&2B	(XB	*�B	.�B	3�B	4�B	6zB	7�B	8�B	9�B	>�B	?�B	A�B	B�B	C�B	C�B	D�B	E�B	E�B	GB	HB	KB	MB	NB	O(B	PB	QB	QB	R:B	S@B	TFB	V9B	WYB	WYB	XyB	Z�B	\�B	c�B	d�B	d�B	f�B	h�B	i�B	k�B	l�B	o�B	p�B	sB	t�B	y	B	{0B	}<B	~(B	~BB	HB	�OB	�[B	�{B	��B	�zB	�lB	��B	�rB	�xB	�xB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	�$B	�1B	�!B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�&B	�&B	�B	�&B	�B	�zB	�XB	�=B	�WB	�CB	�]B	�]B	�cB	��B	��B	�hB	�nB	��B	��B	��B	�"B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�	B	��B	�B	��B	��B	�B	�B	�B	�"B	�(B	�.B	�4B	�@B	�[B	�aB	�SB	�SB	�?B	�9B	�YB	�?B	�?B	�YB	�eB	�QB	�qB	�xB	�jB	�VB	�pB	ߤB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�%B	�B	��B	�B	�B	�	B	�B	�0B	�B	�B	�JB	�6B	�"B	�<B	�B	�(B	�BB	�HB	�}B
;B
AB
aB
GB
MB
gB
SB
9B
9B
SB
mB
YB
YB
zB
�B
�B
	�B
�B
�B
�B
�B
}B
}B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#B
/B
 B
 �B
 �B
"B
!�B
"B
"�B
# B
#B
#B
$B
$&B
%B
%B
&2B
&LB
'8B
'RB
($B
($B
)_B
*KB
+6B
+kB
,=B
,qB
-CB
.cB
.IB
.cB
/iB
/iB
/5B
/OB
/OB
/�B
0�B
1�B
2|B
3�B
3�B
4nB
4nB
4�B
4nB
5�B
5�B
5�B
6`B
6�B
6zB
6zB
7�B
8lB
8lB
8�B
8lB
8lB
8�B
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
;B
;�B
;�B
;�B
<�B
<�B
=�B
=�B
>�B
>�B
?�B
?�B
?�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
IB
IB
J#B
J�B
K�B
K�B
K�B
LB
LB
MB
MB
MB
MB
N"B
NB
NB
N"B
OB
O(B
P.B
QB
Q4B
QB
Q B
Q B
Q B
QB
Q B
Q B
QB
QB
R B
R B
R:B
R B
S&B
SB
SB
SB
SB
SB
S@B
S&B
S@B
S[B
U2B
VB
V9B
VSB
V9B
VSB
W?B
WYB
XEB
X_B
WYB
XEB
YKB
Y1B
YKB
ZkB
ZkB
ZQB
ZQB
[WB
[WB
[=B
[WB
[=B
[WB
[qB
\xB
]dB
\]B
]~B
^jB
^jB
^OB
^jB
^�B
^jB
^�B
_pB
_�B
_�B
`�B
a|B
a�B
a�B
b�B
c�B
cnB
cnB
cnB
c�B
c�B
d�B
c�B
d�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
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
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
p�B
o�B
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
tB
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
u%B
vB
v�B
v�B
wB
xB
w�B
w�B
xB
w�B
w�B
xB
w�B
y	B
x�B
y	B
y	B
y	B
y	B
y�B
zB
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.25(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201902130035192019021300351920190213003519202211182137562022111821375620221118213756201902140017532019021400175320190214001753  JA  ARFMdecpA19c                                                                20190203041720  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190202191853  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190202191855  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190202191856  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190202191856  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190202191856  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190202191857  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190202191857  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190202191857  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190202191857                      G�O�G�O�G�O�                JA  ARUP                                                                        20190202215745                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190202153515  CV  JULD            G�O�G�O�F�%                JM  ARCAJMQC2.0                                                                 20190212153519  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190212153519  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190213151753  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114231518                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123756  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                