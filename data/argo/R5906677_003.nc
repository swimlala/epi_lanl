CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-07-26T11:00:45Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20210726110045  20210726110045  5906677 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  8776                            2B  A   NAVIS_A                         1304                            170425                          863 @ن�\6�1   @نܟJ�@)�C��%�d��/��1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A   @�33@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2�fD3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~�fD  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�p 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���A ��A"ffABffAbffA�33A�33A�33A�33A�33A�33A�33A�33B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C &fC&fC&fC&fC&fC
&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC &fC"&fC$&fC&&fC(&fC*&fC,&fC.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd&fCf&fCh&fCj&fCl&fCn&fCp&fCr&fCt&fCv�Cx&fCz&fC|&fC~&fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D ��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D		�D	��D
	�D
��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D 	�D ��D!	�D!��D"	�D"��D#	�D#��D$	�D$��D%	�D%��D&	�D&��D'	�D'��D(	�D(��D)	�D)��D*	�D*��D+	�D+��D,	�D,��D-	�D-��D.	�D.��D/	�D/��D0	�D0��D1	�D1��D2	�D2� D3	�D3��D4	�D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8��D9	�D9��D:	�D:��D;	�D;��D<	�D<��D=	�D=��D>	�D>��D?	�D?��D@	�D@��DA	�DA��DB	�DB��DC	�DC��DD	�DD��DE	�DE��DF	�DF��DG	�DG��DH	�DH��DI	�DI��DJ	�DJ��DK	�DK��DL	�DL��DM	�DM��DN	�DN��DO	�DO��DP	�DP��DQ	�DQ��DR	�DR��DS	�DS��DT	�DT��DU	�DU��DV	�DV��DW	�DW��DX	�DX��DY	�DY��DZ	�DZ��D[	�D[��D\	�D\��D]	�D]��D^	�D^��D_	�D_��D`	�D`��Da	�Da��Db	�Db��Dc	�Dc��Dd	�Dd��De	�De��Df	�Df��Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj��Dk	�Dk��Dl	�Dl��Dm	�Dm��Dn	�Dn��Do	�Do��Dp	�Dp��Dq	�Dq��Dr	�Dr��Ds	�Ds��Dt	�Dt��Du	�Du��Dv	�Dv��Dw	�Dw��Dx	�Dx��Dy	�Dy��Dz	�Dz��D{	�D{��D|	�D|��D}	�D}��D~	�D~� D	�D��D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D�D���D��D�D�DÄ�D���D��D�D�DĄ�D���D��D�D�Dń�D���D��D�D�DƄ�D���D��D�D�DǄ�D���D��D�D�DȄ�D���D��D�D�DɄ�D���D��D�D�Dʄ�D���D��D�D�D˄�D���D��D�D�D̄�D���D��D�D�D̈́�D���D��D�D�D΄�D���D��D�D�Dτ�D���D��D�D�DЄ�D���D��D�D�Dф�D���D��D�D�D҄�D���D��D�D�Dӄ�D���D��D�D�DԄ�D���D��D�D�DՄ�D���D��D�D�Dք�D���D��D�D�Dׄ�D���D��D�D�D؄�D���D��D�D�Dل�D���D��D�D�Dڄ�D���D��D�D�Dۄ�D���D��D�D�D܄�D���D��D�D�D݄�D���D��D�D�Dބ�D���D��D�D�D߄�D���D��D�D�D���D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D���D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�H D�t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�%A�A�%A�JA�JA�JA�JA�JA�VA�VA�bA�bA�JA��A��yA��yA��;A�ĜAء�A�|�A�Q�A��A׼jAץ�A׏\A׃A�r�A�dZA�/A��AփA�C�A��/Aպ^A���A��AӶFA�A�A�%AξwA�|�A���A�33A���AƬAŰ!A�7LA�hsA�XA�`BA�VA�5?A��hA�^5A�A��#A��+A�1'A���A���A�r�A��A��7A��!A���A���A���A�r�A�p�A���A��A���A��A��FA�;dA�7LA�?}A��
A���A�A�?}A��yA��A���A�JA��+A��FA}t�Aw��At(�Ap��Am��Ah1'Aa�-A_G�AZM�AU��AO�wAI`BAC�AAoA>=qA=�A<��A:��A9VA6��A5?}A3t�A1ƨA1S�A0��A0M�A/�TA/7LA.��A.A�A.bA.JA-��A-�hA-O�A,�uA+��A+�A*�9A*n�A*9XA* �A)�A(-A'�A&ȴA&��A&�uA&z�A&�DA&��A&M�A&$�A&$�A&�A&M�A& �A%�TA%��A%O�A%"�A$�HA$��A$=qA#p�A#�A"�uA!O�A =qA��AK�A�-AE�A1A�A�mA�A;dA�\A  A/A��A�9A^5A1'A�TA�7A�uA�A��AhsA33AQ�A��A�Ap�A33A��A~�AQ�A9XA\)A�9A�A�^AS�AffAdZA�A�+A�A��A+A
��A
�uA
bNA
=qA	��A	�A��AffA$�A�;A��A��A�PAl�A;dA�`Az�AhsA��A��A9XA��A�wA�A\)A�`A�wA�A ��A �9A ��A z�A @�l�@��H@��@��@�ȴ@�-@��u@�l�@�z�@���@�dZ@���@�n�@�M�@��@��#@��@�/@���@�Q�@�|�@���@�ff@�@��@�/@��@�@���@��@�V@�p�@��@�z�@�Z@�Z@�(�@��;@�P@��H@�5?@���@��@��y@��@�Ĝ@�r�@�j@�bN@�(�@�w@�C�@�ff@�hs@�b@޸R@�-@�/@ܣ�@�  @��m@�K�@���@ڏ\@�v�@�5?@٩�@ش9@�"�@��`@�I�@�  @�dZ@��@ҸR@җ�@��@���@с@�&�@��@��;@ύP@�K�@�+@�"�@���@�M�@��T@͑h@�&�@���@�bN@���@�o@ʗ�@�E�@��T@ɡ�@��@ț�@� �@Ǯ@�"�@���@�ȴ@Ɨ�@�~�@�5?@ź^@�?}@ă@�A�@�dZ@\@�@���@�`B@��@��/@�I�@�b@��@��@�o@�M�@��@���@�G�@���@���@�A�@���@�l�@���@���@�O�@��u@�ƨ@�ff@�@���@�`B@���@�(�@��@���@�^5@���@��`@�A�@��F@�l�@�;d@���@���@�ff@�V@�M�@���@��7@���@�r�@�1'@�  @�ƨ@�|�@��@�E�@�@�x�@���@�r�@�(�@�  @��@��;@�ƨ@��w@���@�\)@��y@���@��+@�=q@���@�p�@��@�Ĝ@�A�@� �@��m@�\)@��@���@�{@��#@��-@��h@�?}@��9@�1'@���@���@�33@���@�@���@�Q�@�A�@�(�@���@��@��@�t�@�"�@�ȴ@��\@�V@�@��^@�X@�/@��@��u@�Z@�Z@�I�@�A�@�1'@��@��w@�\)@��@���@���@�G�@���@��D@�r�@�I�@�1'@��m@���@��F@�|�@�dZ@�S�@�@��@�ȴ@���@�E�@�J@��#@�@���@��h@�x�@��@��@���@��9@�bN@�1'@���@���@���@�|�@�\)@�33@�ȴ@���@�~�@�ff@�ff@�^5@�M�@���@���@�O�@�?}@�/@�V@���@�1'@���@��w@�\)@�+@���@�-@�@���@�?}@�%@��/@�Ĝ@��9@��@��@���@��@�l�@���@�v�@��@��^@�&�@���@�b@��@��@��@���@��+@�$�@���@��@�X@�/@��@��@�r�@�b@�1@�@�w@;d@+@K�@~�@~�+@}�@}p�@|��@{��@{�
@{�@{@x�`@xĜ@x�9@x��@xr�@w�@w��@w�P@w|�@wK�@v��@v@u�-@u�h@u?}@u/@t�D@sC�@r��@r�!@r��@r�\@r�\@r~�@r=q@r�@q�#@q��@q��@p�`@p�@pb@ol�@n�y@n�+@nE�@n$�@m�h@l�/@l9X@kt�@kS�@j=q@i�@iG�@h��@h��@h��@hbN@h  @f�+@f$�@f{@e�T@e��@e��@ep�@e�@d��@d�/@d��@d�D@d(�@c��@cdZ@c"�@b��@b��@b~�@bn�@b^5@b^5@b=q@a��@`��@`r�@`Q�@` �@`  @_��@_|�@^��@]�@]�h@]`B@]?}@]V@\�/@\�j@\z�@\Z@\(�@[ƨ@[�@[@ZJ@Y�#@Yx�@YG�@Y%@X�u@W�@W�@W
=@Vv�@U�T@U�-@UO�@T�@T�D@T��@T�@T�@Tz�@TI�@T9X@SS�@S@R~�@R=q@R-@RJ@Q�#@Qx�@PbN@PA�@P �@O�@O��@O;d@Nȴ@Nv�@NV@M�T@M��@M`B@M`B@M?}@M�@MV@L��@L�j@K�m@J�\@J^5@J-@I��@I�7@IX@IG�@I�@H�`@H��@Hr�@H  @G�@G|�@GK�@G+@F�y@F�R@F�+@F$�@E��@E@E��@E`B@EO�@EV@D�/@D�@Dj@C��@C��@B�@B~�@BM�@B�@BJ@A��@Ahs@AG�@@��@@��@@bN@@A�@@  @?\)@>ȴ@>�R@>�+@>V@>5?@>{@=`B@<�@<��@<��@<�@;��@;o@9��@9hs@9&�@9%@8�`@8r�@8 �@7�P@7l�@7K�@7+@7�@6ȴ@6E�@6$�@6{@6@5@5?}@4��@4Z@4�@41@3�
@3��@3��@3��@3�@3S�@333@2��@2M�@2�@1�@1��@1��@1�7@1hs@17L@1�@0Ĝ@0bN@/�P@/;d@.�+@-�@-p�@-/@,��@,�/@,��@,�j@,�j@,�@,�@+�m@+ƨ@+��@+t�@+o@*��@*�!@*�\@*�\@*n�@)�#@)X@)G�@)&�@(�`@(�9@(�u@(r�@(A�@(1'@(b@'�;@'�w@'��@'�P@'\)@';d@'�@&�y@&�R@&v�@&{@&@%�T@%�-@%O�@%V@$�/@$j@$Z@$I�@$9X@$(�@$�@$�@$1@#��@#�F@#"�@"�H@"�\@"J@!hs@ �`@ r�@ bN@ A�@   @�;@�P@|�@l�@\)@\)@;d@+@��@ȴ@��@{@�@��@`B@/@/@V@�j@�D@z�@9X@�
@�F@�@dZ@S�@C�@o@o@�@�@��@��@�\@�\@n�@��@�#@��@x�@&�@��@��@��@�@  @��@�P@\)@+@�@�@��@�y@�R@�+@E�@{@��@�@p�@?}@V@�/@�/@�@j@I�@(�@1@1@1@1@�m@ƨ@�F@��@S�@�H@�\@�\@-@��@��@��@��@7L@&�@&�@�@�`@bN@1'@  @  @�;@�w@�w@��@��@��@�@��@�@�P@\)@
=@�R@�+@ff@$�@@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�%A�A�%A�JA�JA�JA�JA�JA�VA�VA�bA�bA�JA��A��yA��yA��;A�ĜAء�A�|�A�Q�A��A׼jAץ�A׏\A׃A�r�A�dZA�/A��AփA�C�A��/Aպ^A���A��AӶFA�A�A�%AξwA�|�A���A�33A���AƬAŰ!A�7LA�hsA�XA�`BA�VA�5?A��hA�^5A�A��#A��+A�1'A���A���A�r�A��A��7A��!A���A���A���A�r�A�p�A���A��A���A��A��FA�;dA�7LA�?}A��
A���A�A�?}A��yA��A���A�JA��+A��FA}t�Aw��At(�Ap��Am��Ah1'Aa�-A_G�AZM�AU��AO�wAI`BAC�AAoA>=qA=�A<��A:��A9VA6��A5?}A3t�A1ƨA1S�A0��A0M�A/�TA/7LA.��A.A�A.bA.JA-��A-�hA-O�A,�uA+��A+�A*�9A*n�A*9XA* �A)�A(-A'�A&ȴA&��A&�uA&z�A&�DA&��A&M�A&$�A&$�A&�A&M�A& �A%�TA%��A%O�A%"�A$�HA$��A$=qA#p�A#�A"�uA!O�A =qA��AK�A�-AE�A1A�A�mA�A;dA�\A  A/A��A�9A^5A1'A�TA�7A�uA�A��AhsA33AQ�A��A�Ap�A33A��A~�AQ�A9XA\)A�9A�A�^AS�AffAdZA�A�+A�A��A+A
��A
�uA
bNA
=qA	��A	�A��AffA$�A�;A��A��A�PAl�A;dA�`Az�AhsA��A��A9XA��A�wA�A\)A�`A�wA�A ��A �9A ��A z�A @�l�@��H@��@��@�ȴ@�-@��u@�l�@�z�@���@�dZ@���@�n�@�M�@��@��#@��@�/@���@�Q�@�|�@���@�ff@�@��@�/@��@�@���@��@�V@�p�@��@�z�@�Z@�Z@�(�@��;@�P@��H@�5?@���@��@��y@��@�Ĝ@�r�@�j@�bN@�(�@�w@�C�@�ff@�hs@�b@޸R@�-@�/@ܣ�@�  @��m@�K�@���@ڏ\@�v�@�5?@٩�@ش9@�"�@��`@�I�@�  @�dZ@��@ҸR@җ�@��@���@с@�&�@��@��;@ύP@�K�@�+@�"�@���@�M�@��T@͑h@�&�@���@�bN@���@�o@ʗ�@�E�@��T@ɡ�@��@ț�@� �@Ǯ@�"�@���@�ȴ@Ɨ�@�~�@�5?@ź^@�?}@ă@�A�@�dZ@\@�@���@�`B@��@��/@�I�@�b@��@��@�o@�M�@��@���@�G�@���@���@�A�@���@�l�@���@���@�O�@��u@�ƨ@�ff@�@���@�`B@���@�(�@��@���@�^5@���@��`@�A�@��F@�l�@�;d@���@���@�ff@�V@�M�@���@��7@���@�r�@�1'@�  @�ƨ@�|�@��@�E�@�@�x�@���@�r�@�(�@�  @��@��;@�ƨ@��w@���@�\)@��y@���@��+@�=q@���@�p�@��@�Ĝ@�A�@� �@��m@�\)@��@���@�{@��#@��-@��h@�?}@��9@�1'@���@���@�33@���@�@���@�Q�@�A�@�(�@���@��@��@�t�@�"�@�ȴ@��\@�V@�@��^@�X@�/@��@��u@�Z@�Z@�I�@�A�@�1'@��@��w@�\)@��@���@���@�G�@���@��D@�r�@�I�@�1'@��m@���@��F@�|�@�dZ@�S�@�@��@�ȴ@���@�E�@�J@��#@�@���@��h@�x�@��@��@���@��9@�bN@�1'@���@���@���@�|�@�\)@�33@�ȴ@���@�~�@�ff@�ff@�^5@�M�@���@���@�O�@�?}@�/@�V@���@�1'@���@��w@�\)@�+@���@�-@�@���@�?}@�%@��/@�Ĝ@��9@��@��@���@��@�l�@���@�v�@��@��^@�&�@���@�b@��@��@��@���@��+@�$�@���@��@�X@�/@��@��@�r�@�b@�1@�@�w@;d@+@K�@~�@~�+@}�@}p�@|��@{��@{�
@{�@{@x�`@xĜ@x�9@x��@xr�@w�@w��@w�P@w|�@wK�@v��@v@u�-@u�h@u?}@u/@t�D@sC�@r��@r�!@r��@r�\@r�\@r~�@r=q@r�@q�#@q��@q��@p�`@p�@pb@ol�@n�y@n�+@nE�@n$�@m�h@l�/@l9X@kt�@kS�@j=q@i�@iG�@h��@h��@h��@hbN@h  @f�+@f$�@f{@e�T@e��@e��@ep�@e�@d��@d�/@d��@d�D@d(�@c��@cdZ@c"�@b��@b��@b~�@bn�@b^5@b^5@b=q@a��@`��@`r�@`Q�@` �@`  @_��@_|�@^��@]�@]�h@]`B@]?}@]V@\�/@\�j@\z�@\Z@\(�@[ƨ@[�@[@ZJ@Y�#@Yx�@YG�@Y%@X�u@W�@W�@W
=@Vv�@U�T@U�-@UO�@T�@T�D@T��@T�@T�@Tz�@TI�@T9X@SS�@S@R~�@R=q@R-@RJ@Q�#@Qx�@PbN@PA�@P �@O�@O��@O;d@Nȴ@Nv�@NV@M�T@M��@M`B@M`B@M?}@M�@MV@L��@L�j@K�m@J�\@J^5@J-@I��@I�7@IX@IG�@I�@H�`@H��@Hr�@H  @G�@G|�@GK�@G+@F�y@F�R@F�+@F$�@E��@E@E��@E`B@EO�@EV@D�/@D�@Dj@C��@C��@B�@B~�@BM�@B�@BJ@A��@Ahs@AG�@@��@@��@@bN@@A�@@  @?\)@>ȴ@>�R@>�+@>V@>5?@>{@=`B@<�@<��@<��@<�@;��@;o@9��@9hs@9&�@9%@8�`@8r�@8 �@7�P@7l�@7K�@7+@7�@6ȴ@6E�@6$�@6{@6@5@5?}@4��@4Z@4�@41@3�
@3��@3��@3��@3�@3S�@333@2��@2M�@2�@1�@1��@1��@1�7@1hs@17L@1�@0Ĝ@0bN@/�P@/;d@.�+@-�@-p�@-/@,��@,�/@,��@,�j@,�j@,�@,�@+�m@+ƨ@+��@+t�@+o@*��@*�!@*�\@*�\@*n�@)�#@)X@)G�@)&�@(�`@(�9@(�u@(r�@(A�@(1'@(b@'�;@'�w@'��@'�P@'\)@';d@'�@&�y@&�R@&v�@&{@&@%�T@%�-@%O�@%V@$�/@$j@$Z@$I�@$9X@$(�@$�@$�@$1@#��@#�F@#"�@"�H@"�\@"J@!hs@ �`@ r�@ bN@ A�@   @�;@�P@|�@l�@\)@\)@;d@+@��@ȴ@��@{@�@��@`B@/@/@V@�j@�D@z�@9X@�
@�F@�@dZ@S�@C�@o@o@�@�@��@��@�\@�\@n�@��@�#@��@x�@&�@��@��@��@�@  @��@�P@\)@+@�@�@��@�y@�R@�+@E�@{@��@�@p�@?}@V@�/@�/@�@j@I�@(�@1@1@1@1@�m@ƨ@�F@��@S�@�H@�\@�\@-@��@��@��@��@7L@&�@&�@�@�`@bN@1'@  @  @�;@�w@�w@��@��@��@�@��@�@�P@\)@
=@�R@�+@ff@$�@@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�hB
�=B
�B
~�B
{�B
q�B
ffB
aHB
ZB
H�B
6FB
<jB
?}B
J�B
R�B
l�B
�7B
��B
�B
ŢB
�B
�B
��B%B�B1'BF�BaHB�B�B�JB��B�{Bu�Bw�Bs�BC�B%�B!�BB�BbNBbNBL�B7LB
=B
�sB
��B
�jB
�LB
�}B
�B
�uB
�+B
t�B
YB
<jB
�B
DB	�B	ĜB	�3B	��B	�oB	}�B	^5B	R�B	?}B	,B	 �B	�B	DB	+B	uB	"�B	&�B	?}B	O�B	r�B	�B	��B	��B	��B	��B	�B	�B	�'B	�3B	�9B	��B	��B	�
B	�NB	�HB	�BB	�BB	�TB	�ZB	�ZB	�`B	�ZB	�B	�B	�B	�B	�B	�B	�B	��B	��B
B
\B
�B
�B
'�B
2-B
;dB
C�B
E�B
E�B
E�B
H�B
H�B
E�B
C�B
>wB
7LB
0!B
+B
)�B
,B
.B
49B
9XB
<jB
=qB
=qB
=qB
=qB
;dB
9XB
8RB
8RB
8RB
;dB
<jB
;dB
9XB
<jB
>wB
<jB
<jB
=qB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
<jB
9XB
5?B
33B
2-B
/B
,B
+B
)�B
)�B
)�B
(�B
(�B
(�B
(�B
'�B
'�B
)�B
(�B
(�B
'�B
'�B
'�B
'�B
&�B
&�B
&�B
%�B
$�B
$�B
$�B
%�B
&�B
&�B
%�B
$�B
#�B
#�B
$�B
!�B
�B
�B
 �B
 �B
"�B
#�B
$�B
#�B
#�B
#�B
#�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
oB
hB
hB
hB
hB
hB
bB
\B
PB
DB
+B
%B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
B
  B
  B
B
  B
B
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
B
B
B
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
%B
+B
+B
1B
1B

=B
	7B

=B
	7B

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
PB
PB
PB
PB
JB
JB
PB
PB
PB
PB
PB
PB
PB
VB
VB
\B
VB
\B
bB
bB
hB
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
oB
oB
uB
uB
uB
{B
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
 �B
!�B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
+B
+B
+B
,B
,B
,B
-B
-B
-B
.B
.B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
2-B
1'B
2-B
33B
33B
49B
5?B
5?B
5?B
6FB
5?B
5?B
5?B
5?B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
8RB
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
;dB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
>wB
>wB
>wB
>wB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
F�B
G�B
G�B
H�B
H�B
G�B
G�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
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
M�B
M�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
P�B
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
R�B
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
T�B
T�B
T�B
VB
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
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
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
e`B
e`B
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
hsB
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
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
s�B
s�B
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
u�B
v�B
v�B
v�B
v�B
v�B
v�B
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
x�B
x�B
y�B
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
z�B
z�B
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
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�hB
�=B
�B
~�B
{�B
q�B
ffB
aHB
ZB
H�B
6FB
<jB
?}B
J�B
R�B
l�B
�7B
��B
�B
ŢB
�B
�B
��B%B�B1'BF�BaHB�B�B�JB��B�{Bu�Bw�Bs�BC�B%�B!�BB�BbNBbNBL�B7LB
=B
�sB
��B
�jB
�LB
�}B
�B
�uB
�+B
t�B
YB
<jB
�B
DB	�B	ĜB	�3B	��B	�oB	}�B	^5B	R�B	?}B	,B	 �B	�B	DB	+B	uB	"�B	&�B	?}B	O�B	r�B	�B	��B	��B	��B	��B	�B	�B	�'B	�3B	�9B	��B	��B	�
B	�NB	�HB	�BB	�BB	�TB	�ZB	�ZB	�`B	�ZB	�B	�B	�B	�B	�B	�B	�B	��B	��B
B
\B
�B
�B
'�B
2-B
;dB
C�B
E�B
E�B
E�B
H�B
H�B
E�B
C�B
>wB
7LB
0!B
+B
)�B
,B
.B
49B
9XB
<jB
=qB
=qB
=qB
=qB
;dB
9XB
8RB
8RB
8RB
;dB
<jB
;dB
9XB
<jB
>wB
<jB
<jB
=qB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
<jB
9XB
5?B
33B
2-B
/B
,B
+B
)�B
)�B
)�B
(�B
(�B
(�B
(�B
'�B
'�B
)�B
(�B
(�B
'�B
'�B
'�B
'�B
&�B
&�B
&�B
%�B
$�B
$�B
$�B
%�B
&�B
&�B
%�B
$�B
#�B
#�B
$�B
!�B
�B
�B
 �B
 �B
"�B
#�B
$�B
#�B
#�B
#�B
#�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
oB
hB
hB
hB
hB
hB
bB
\B
PB
DB
+B
%B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
B
  B
  B
B
  B
B
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
B
B
B
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
%B
+B
+B
1B
1B

=B
	7B

=B
	7B

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
PB
PB
PB
PB
JB
JB
PB
PB
PB
PB
PB
PB
PB
VB
VB
\B
VB
\B
bB
bB
hB
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
oB
oB
uB
uB
uB
{B
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
 �B
!�B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
+B
+B
+B
,B
,B
,B
-B
-B
-B
.B
.B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
2-B
1'B
2-B
33B
33B
49B
5?B
5?B
5?B
6FB
5?B
5?B
5?B
5?B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
8RB
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
;dB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
>wB
>wB
>wB
>wB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
F�B
G�B
G�B
H�B
H�B
G�B
G�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
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
M�B
M�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
P�B
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
R�B
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
T�B
T�B
T�B
VB
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
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
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
e`B
e`B
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
hsB
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
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
s�B
s�B
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
u�B
v�B
v�B
v�B
v�B
v�B
v�B
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
x�B
x�B
y�B
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
z�B
z�B
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
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.15 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210726110045                              AO  ARCAADJP                                                                    20210726110045    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210726110045  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210726110045  QCF$                G�O�G�O�G�O�0               