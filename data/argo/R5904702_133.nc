CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-06-08T22:31:12Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         C   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    9   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    9    HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    9$   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    9(   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    98   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    9H   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    9X   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  9`   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  9�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  @  9�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        :    	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    :$   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    :(   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     :,   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    :L   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    :P   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     :T   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     :t   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     :�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    :�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_NB_SAMPLE_CTD_QC               	long_name         ,Global quality flag of NB_SAMPLE_CTD profile   conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        p  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Kl   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  OH   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ^�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  b�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  r   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �P   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  Τ   NB_SAMPLE_CTD            
         	long_name         2Number of samples in each pressure bin for the CTD     
_FillValue        �     units         count      C_format      %5d    FORTRAN_format        I5     
resolution                �  �   NB_SAMPLE_CTD_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  @  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  8  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �(   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �0   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230608223112  20230608223112  5904702 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            NB_SAMPLE_CTD      �A   AO  5449                            2B  A   NAVIS_A                         0470                            011514                          863 @خ��ng�1   @خ��bb@8%�Q��d�l�C��1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F       @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dzy�D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�<�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�  @���AffA"ffABffAbffA�33A�33A�33A�33A�33A�33A�33A�33B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C &fC&fC&fC&fC&fC
&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC &fC"&fC$&fC&&fC(&fC*&fC,&fC.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd&fCf&fCh&fCj&fCl&fCn&fCp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D ��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D		�D	��D
	�D
��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D 	�D ��D!	�D!��D"	�D"��D#	�D#��D$	�D$��D%	�D%��D&	�D&��D'	�D'��D(	�D(��D)	�D)��D*	�D*��D+	�D+��D,	�D,��D-	�D-��D.	�D.��D/	�D/��D0	�D0��D1	�D1��D2	�D2��D3	�D3��D4	�D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8��D9	�D9��D:	�D:��D;	�D;��D<	�D<��D=	�D=��D>	�D>��D?	�D?��D@	�D@��DA	�DA��DB	�DB��DC	�DC��DD	�DD��DE	�DE��DF	�DF��DG	�DG��DH	�DH��DI	�DI��DJ	�DJ��DK	�DK��DL	�DL��DM	�DM��DN	�DN��DO	�DO��DP	�DP��DQ	�DQ��DR	�DR��DS	�DS��DT	�DT��DU	�DU��DV	�DV��DW	�DW��DX	�DX��DY DY��DZ	�DZ��D[	�D[��D\	�D\��D]	�D]��D^	�D^��D_	�D_��D`	�D`��Da	�Da��Db	�Db��Dc	�Dc��Dd	�Dd��De	�De��Df	�Df��Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj��Dk	�Dk��Dl	�Dl��Dm	�Dm��Dn	�Dn��Do	�Do��Dp	�Dp��Dq	�Dq��Dr	�Dr��Ds	�Ds��Dt	�Dt��Du	�Du��Dv	�Dv��Dw	�Dw��Dx	�Dx��Dy	�Dy��Dz	�Dz�4D{	�D{��D|	�D|��D}	�D}��D~	�D~��D	�D��D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�A�D���D���D��D�D�D���D���D� D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D���D���D��D�D�D�D���D��D�D�DÄ�D���D��D�D�DĄ�D���D��D�D�Dń�D���D��D�D�DƄ�D���D��D�D�DǄ�D���D��D�D�DȄ�D���D��D�D�DɄ�D���D��D�D�Dʄ�D���D��D�D�D˄�D���D��D�D�D̄�D���D��D�D�D̈́�D���D��D�D�D΄�D���D��D�D�Dτ�D���D��D�D�DЄ�D���D��D�D�Dф�D���D��D�D�D҄�D���D��D�D�Dӄ�D���D��D�D�DԄ�D���D��D�D�DՄ�D���D��D�D�Dք�D���D��D�D�Dׄ�D���D��D�D�D؄�D���D��D�D�Dل�D���D��D�D�Dڄ�D���D��D�D�Dۄ�D���D��D�D�D܄�D���D��D�D�D݄�D���D��D�D�Dބ�D���D��D�D�D߄�D���D��D�D�D���D���D��D�D�D� D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D���D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D��D���D��D�D�D���D���D��D�D�D�� D���D��D�A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��uA��DA�n�A��A��\A�9XA�&�A��A�{A�1A�A���A���A��A��HA���A�A��A���A��A�hsA�ZA�Q�A�A�A�9XA�33A�33A�/A�%A��mA���A��RA��A���A���A�r�A�bNA�XA�S�A�S�A�Q�A�M�A�M�A�I�A�G�A�C�A�9XA�&�A��A�oA�%A���A���A���A���A��A��/A�A���A�33A��A�=qA�bNA�=qA��A���A�$�A�l�A���A��yA��A��A��9A�`BA�A��RA�&�A���A�ffA��/A��A��A���A��hA��#A��uA�M�A���A�M�A�K�A��wA���A�l�A�VA���A�M�A��A�1A��A� �A��A���A��A�(�A���A�I�A��hA��A�z�A�-A�bA��PA�33A�"�A�9XA�E�A���A��A�ĜA���A��A�t�A�O�A� �A���A�M�A���A�C�A���A�;dA�\)A7LA|r�A{"�Aw"�AsS�AqVAo�FAlE�AjZAi��Ai�Ah�Af��AeK�Ad�yAdffAcXAa�-A`bNA_��A^��A[�^AZ�AZffAZ�AY�TAY��AY33AVr�ARffAPI�AMt�AI�PAG�mAF�DAE�AD��AD�AB��AAdZA@��A?|�A?%A>A<�`A<A;�-A;dZA:-A9
=A8bA7��A7�7A7�A7`BA7`BA6A�A45?A2��A1dZA0v�A0ZA0M�A0A�A0=qA05?A0{A/ƨA/��A/�A,��A,n�A,A�A+VA(��A&9XA%�;A%l�A$�A$�!A$jA#&�A!|�A ��A 9XAG�AbNA��A�A�HA��AQ�AƨAhsAC�A�hA�A�A��A�9A��A�A9XA�;AXAjAQ�A�A��A�AbNAS�A
��A
(�A	�^A	"�A��AQ�A��AA�A7LA%A��A9XA�A�!AG�A ~�@��^@��;@���@���@�j@�n�@���@�(�@�!@�=q@�Ĝ@��@��@�V@�@��@�Q�@�(�@���@�$�@�h@�9@�  @�S�@��H@�J@���@�z�@� �@��y@�@�j@���@�^5@���@�%@�A�@�dZ@�$�@Ѻ^@�`B@��@�1'@�\)@�V@��m@˅@��@�r�@��
@Ƨ�@��@ũ�@�/@ċD@���@�;d@��y@§�@�~�@�~�@�=q@��-@�G�@��@��@��
@�t�@�$�@���@�7L@��/@�9X@���@�+@��!@�E�@�x�@��@���@��/@�I�@��H@�M�@��@��@�&�@���@��@���@�A�@��@�\)@���@��@��/@�;d@��@���@��^@��/@�33@��H@���@��\@�ff@�@���@���@���@��`@���@�z�@��@��@��@���@��h@�%@���@��@�j@�Q�@��m@���@�l�@�"�@��@��!@��#@�7L@�V@���@���@��
@���@�v�@�M�@��7@��@��`@�dZ@���@�-@�G�@��/@���@���@���@���@���@��u@�Z@�|�@���@�J@�l�@�n�@��@�@���@��@�O�@��@�Ĝ@�  @���@���@�|�@�S�@��@��@��`@�Z@�|�@��y@��@��\@��@��h@���@��u@�I�@��m@��w@��F@���@�;d@�v�@�E�@�J@���@���@��@�hs@�`B@�G�@��@��@��9@���@�Q�@��@K�@}��@|9X@{C�@z��@y��@yX@x�9@x�@xb@wK�@v�y@v��@v$�@up�@t�j@s��@r�@rn�@r�@q�#@q��@q�7@p�`@p�@o�w@n��@nff@n{@m�@l1@kdZ@kS�@k33@kC�@k33@k33@k"�@ko@ko@ko@ko@ko@ko@k@j��@j�@i%@hb@g�@g�@g;d@f��@e��@eO�@e/@d��@dZ@dI�@d(�@c�@b��@bM�@b�@a��@a�#@a�^@a��@a�7@a�7@ahs@ahs@aX@a7L@a%@`Ĝ@`�u@`A�@` �@_�@_�;@_�;@_�;@_�;@_�;@_�;@_�;@_��@_�w@_�@_�P@_\)@_
=@^�R@^5?@]�h@]p�@]`B@]`B@]`B@]`B@]?}@]?}@\��@[�@[C�@[C�@[@Z-@Y��@X��@XQ�@Xb@W�@W+@V�@VV@U�T@T�/@T9X@S33@R��@Q�#@Q7L@Q7L@P�9@O��@Pb@PbN@O�P@M`B@LI�@Lj@Kƨ@KC�@Ko@J�!@JJ@I��@I��@I7L@I&�@H��@H�@Hb@G�w@G��@G�P@G|�@G\)@G;d@F�@Fv�@Fff@F$�@F@E�@E�@E��@E�@EV@D�D@D9X@D�@D�@C�
@Bn�@BM�@B�@A�#@A��@A7L@@�9@@A�@?�w@?|�@>��@>�+@>5?@=��@=?}@<��@<��@<z�@<I�@;�m@;dZ@;@:��@:�!@:n�@:n�@:M�@:�@9��@9�^@9hs@9�@8�`@8��@8�@8bN@8bN@8A�@7�;@7��@7�w@7�@7|�@7l�@7\)@7K�@7+@6�@6ȴ@6��@6��@6ff@6$�@6{@6@5�h@5`B@5/@4�@4z�@4I�@41@3S�@2��@2�!@1��@1%@0�9@0 �@/��@.�y@.ȴ@.�R@.��@.��@.�+@.�+@.v�@.v�@.v�@.v�@.ff@.{@-`B@-?}@-?}@,�j@,9X@+��@+��@+S�@+"�@+o@+o@+o@*�@*�H@*��@)��@(�9@)7L@)�@(1'@'�@'�@'�@'�;@'�;@'�;@'�;@'�;@'�;@'�@';d@&ȴ@&��@&V@&{@&@&@%�T@%p�@$�/@$I�@$9X@$�@$�@#��@#��@#��@#�m@#�m@#�F@#dZ@#33@#"�@#o@"��@"��@"�\@"~�@"-@"J@!��@!�#@!�^@ ��@�@�P@l�@\)@+@��@�R@ff@V@5?@$�@��@�h@V@��@�j@�@��@�D@z�@j@j@Z@I�@�
@dZ@"�@��@-@��@��@�@�@�#@��@X@&�@�9@bN@A�@1'@  @�w@�P@;d@��@ȴ@�R@��@��@��@�+@v�@ff@E�@E�@@��@�-@�h@p�@?}@V@��@Z@9X@(�@�@�@�m@�F@��@��@�@dZ@C�@"�@@�H@�\@M�@-@-@�@�@J@��@�@�@�@�@�#@��@7L@%@�`@��@��@�9@b@�@�y@�R@��@��@��@��@�+@�+@v�@v�@V@E�@$�@@/@��@��@j@Z@Z@9X@��@�
@�
@ƨ@ƨ@ƨ@ƨ@�F@�F@�@t�@dZ@S�@S�@C�@@
��@
�\@
~�@
�\@
M�@
-@	�^@	7L@	&�@	%@��@��@�u@r�@A�@ �@�@�;@�;@��@K�@�y@�R@�+@v�@v�@v�@v�@v�@v�@v�@ff@ff@ff@V@V@E�@$�@{@@�@�@�T@��@��@��@�-@�-@�@/@V@��@�@��@�@�D@z�@I�@(�@ƨ@�@t�@dZ@C�@"�@@��@��@�!@��@~�@M�@J@��@&�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��uA��DA�n�A��A��\A�9XA�&�A��A�{A�1A�A���A���A��A��HA���A�A��A���A��A�hsA�ZA�Q�A�A�A�9XA�33A�33A�/A�%A��mA���A��RA��A���A���A�r�A�bNA�XA�S�A�S�A�Q�A�M�A�M�A�I�A�G�A�C�A�9XA�&�A��A�oA�%A���A���A���A���A��A��/A�A���A�33A��A�=qA�bNA�=qA��A���A�$�A�l�A���A��yA��A��A��9A�`BA�A��RA�&�A���A�ffA��/A��A��A���A��hA��#A��uA�M�A���A�M�A�K�A��wA���A�l�A�VA���A�M�A��A�1A��A� �A��A���A��A�(�A���A�I�A��hA��A�z�A�-A�bA��PA�33A�"�A�9XA�E�A���A��A�ĜA���A��A�t�A�O�A� �A���A�M�A���A�C�A���A�;dA�\)A7LA|r�A{"�Aw"�AsS�AqVAo�FAlE�AjZAi��Ai�Ah�Af��AeK�Ad�yAdffAcXAa�-A`bNA_��A^��A[�^AZ�AZffAZ�AY�TAY��AY33AVr�ARffAPI�AMt�AI�PAG�mAF�DAE�AD��AD�AB��AAdZA@��A?|�A?%A>A<�`A<A;�-A;dZA:-A9
=A8bA7��A7�7A7�A7`BA7`BA6A�A45?A2��A1dZA0v�A0ZA0M�A0A�A0=qA05?A0{A/ƨA/��A/�A,��A,n�A,A�A+VA(��A&9XA%�;A%l�A$�A$�!A$jA#&�A!|�A ��A 9XAG�AbNA��A�A�HA��AQ�AƨAhsAC�A�hA�A�A��A�9A��A�A9XA�;AXAjAQ�A�A��A�AbNAS�A
��A
(�A	�^A	"�A��AQ�A��AA�A7LA%A��A9XA�A�!AG�A ~�@��^@��;@���@���@�j@�n�@���@�(�@�!@�=q@�Ĝ@��@��@�V@�@��@�Q�@�(�@���@�$�@�h@�9@�  @�S�@��H@�J@���@�z�@� �@��y@�@�j@���@�^5@���@�%@�A�@�dZ@�$�@Ѻ^@�`B@��@�1'@�\)@�V@��m@˅@��@�r�@��
@Ƨ�@��@ũ�@�/@ċD@���@�;d@��y@§�@�~�@�~�@�=q@��-@�G�@��@��@��
@�t�@�$�@���@�7L@��/@�9X@���@�+@��!@�E�@�x�@��@���@��/@�I�@��H@�M�@��@��@�&�@���@��@���@�A�@��@�\)@���@��@��/@�;d@��@���@��^@��/@�33@��H@���@��\@�ff@�@���@���@���@��`@���@�z�@��@��@��@���@��h@�%@���@��@�j@�Q�@��m@���@�l�@�"�@��@��!@��#@�7L@�V@���@���@��
@���@�v�@�M�@��7@��@��`@�dZ@���@�-@�G�@��/@���@���@���@���@���@��u@�Z@�|�@���@�J@�l�@�n�@��@�@���@��@�O�@��@�Ĝ@�  @���@���@�|�@�S�@��@��@��`@�Z@�|�@��y@��@��\@��@��h@���@��u@�I�@��m@��w@��F@���@�;d@�v�@�E�@�J@���@���@��@�hs@�`B@�G�@��@��@��9@���@�Q�@��@K�@}��@|9X@{C�@z��@y��@yX@x�9@x�@xb@wK�@v�y@v��@v$�@up�@t�j@s��@r�@rn�@r�@q�#@q��@q�7@p�`@p�@o�w@n��@nff@n{@m�@l1@kdZ@kS�@k33@kC�@k33@k33@k"�@ko@ko@ko@ko@ko@ko@k@j��@j�@i%@hb@g�@g�@g;d@f��@e��@eO�@e/@d��@dZ@dI�@d(�@c�@b��@bM�@b�@a��@a�#@a�^@a��@a�7@a�7@ahs@ahs@aX@a7L@a%@`Ĝ@`�u@`A�@` �@_�@_�;@_�;@_�;@_�;@_�;@_�;@_�;@_��@_�w@_�@_�P@_\)@_
=@^�R@^5?@]�h@]p�@]`B@]`B@]`B@]`B@]?}@]?}@\��@[�@[C�@[C�@[@Z-@Y��@X��@XQ�@Xb@W�@W+@V�@VV@U�T@T�/@T9X@S33@R��@Q�#@Q7L@Q7L@P�9@O��@Pb@PbN@O�P@M`B@LI�@Lj@Kƨ@KC�@Ko@J�!@JJ@I��@I��@I7L@I&�@H��@H�@Hb@G�w@G��@G�P@G|�@G\)@G;d@F�@Fv�@Fff@F$�@F@E�@E�@E��@E�@EV@D�D@D9X@D�@D�@C�
@Bn�@BM�@B�@A�#@A��@A7L@@�9@@A�@?�w@?|�@>��@>�+@>5?@=��@=?}@<��@<��@<z�@<I�@;�m@;dZ@;@:��@:�!@:n�@:n�@:M�@:�@9��@9�^@9hs@9�@8�`@8��@8�@8bN@8bN@8A�@7�;@7��@7�w@7�@7|�@7l�@7\)@7K�@7+@6�@6ȴ@6��@6��@6ff@6$�@6{@6@5�h@5`B@5/@4�@4z�@4I�@41@3S�@2��@2�!@1��@1%@0�9@0 �@/��@.�y@.ȴ@.�R@.��@.��@.�+@.�+@.v�@.v�@.v�@.v�@.ff@.{@-`B@-?}@-?}@,�j@,9X@+��@+��@+S�@+"�@+o@+o@+o@*�@*�H@*��@)��@(�9@)7L@)�@(1'@'�@'�@'�@'�;@'�;@'�;@'�;@'�;@'�;@'�@';d@&ȴ@&��@&V@&{@&@&@%�T@%p�@$�/@$I�@$9X@$�@$�@#��@#��@#��@#�m@#�m@#�F@#dZ@#33@#"�@#o@"��@"��@"�\@"~�@"-@"J@!��@!�#@!�^@ ��@�@�P@l�@\)@+@��@�R@ff@V@5?@$�@��@�h@V@��@�j@�@��@�D@z�@j@j@Z@I�@�
@dZ@"�@��@-@��@��@�@�@�#@��@X@&�@�9@bN@A�@1'@  @�w@�P@;d@��@ȴ@�R@��@��@��@�+@v�@ff@E�@E�@@��@�-@�h@p�@?}@V@��@Z@9X@(�@�@�@�m@�F@��@��@�@dZ@C�@"�@@�H@�\@M�@-@-@�@�@J@��@�@�@�@�@�#@��@7L@%@�`@��@��@�9@b@�@�y@�R@��@��@��@��@�+@�+@v�@v�@V@E�@$�@@/@��@��@j@Z@Z@9X@��@�
@�
@ƨ@ƨ@ƨ@ƨ@�F@�F@�@t�@dZ@S�@S�@C�@@
��@
�\@
~�@
�\@
M�@
-@	�^@	7L@	&�@	%@��@��@�u@r�@A�@ �@�@�;@�;@��@K�@�y@�R@�+@v�@v�@v�@v�@v�@v�@v�@ff@ff@ff@V@V@E�@$�@{@@�@�@�T@��@��@��@�-@�-@�@/@V@��@�@��@�@�D@z�@I�@(�@ƨ@�@t�@dZ@C�@"�@@��@��@�!@��@~�@M�@J@��@&�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
=B
=B	7B
=BDBJBPBVBVB\B\BhBoBuB�B�B�B�B�B�B!�B#�B%�B'�B(�B)�B)�B)�B/B2-B9XBB�BJ�BO�BR�BT�BVBVBVBVBW
BW
BW
BW
BW
BVBVBT�BT�BT�BT�BVBVBW
BXBW
BVBT�BS�BP�BM�BL�BP�BQ�BR�BR�BQ�BN�BJ�BH�BF�B@�B:^B2-B&�B"�B%�B#�B �B�B{B  B�B�mB�BB��BɺBB�jB�3B��B��B�oB�VB�DB�+B�B�Bw�BdZBZBVBR�BL�BE�B>wB)�BoBB  B��B��B�B�5B��B�B��B�VB�JB�=B�1B�+B�B�B}�Br�B_;BW
BP�BE�B9XB.B�BhB
��B
�TB
��B
��B
�FB
�B
��B
��B
��B
�uB
�JB
�=B
�%B
~�B
u�B
n�B
iyB
aHB
R�B
N�B
K�B
I�B
H�B
E�B
?}B
0!B
�B
oB
B	�B	�fB	�HB	�/B	�
B	��B	��B	ȴB	ŢB	ÖB	��B	�qB	�XB	�?B	�9B	�'B	�B	�B	�!B	�-B	�?B	�?B	�FB	�?B	�-B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�hB	�VB	�=B	�B	{�B	y�B	x�B	v�B	u�B	r�B	n�B	jB	hsB	ffB	cTB	aHB	_;B	^5B	]/B	\)B	ZB	XB	VB	O�B	L�B	K�B	J�B	G�B	D�B	@�B	<jB	;dB	:^B	9XB	8RB	7LB	6FB	5?B	49B	33B	2-B	1'B	1'B	0!B	/B	/B	.B	,B	+B	+B	)�B	)�B	(�B	&�B	%�B	%�B	#�B	#�B	#�B	#�B	"�B	!�B	 �B	"�B	"�B	#�B	#�B	$�B	#�B	"�B	 �B	�B	!�B	"�B	!�B	"�B	!�B	 �B	 �B	 �B	 �B	 �B	!�B	"�B	"�B	!�B	"�B	$�B	)�B	.B	/B	/B	0!B	.B	-B	0!B	0!B	1'B	33B	8RB	9XB	=qB	@�B	@�B	B�B	D�B	C�B	C�B	G�B	J�B	K�B	K�B	L�B	N�B	N�B	O�B	O�B	P�B	Q�B	R�B	S�B	S�B	T�B	T�B	T�B	YB	[#B	]/B	]/B	\)B	[#B	^5B	aHB	bNB	e`B	e`B	e`B	dZB	e`B	jB	l�B	m�B	n�B	o�B	p�B	p�B	p�B	q�B	r�B	s�B	t�B	z�B	z�B	}�B	z�B	w�B	u�B	s�B	q�B	q�B	q�B	q�B	q�B	r�B	r�B	r�B	r�B	u�B	u�B	w�B	x�B	z�B	|�B	~�B	�B	�B	�B	�%B	�%B	�+B	�7B	�=B	�JB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�'B	�?B	�LB	�LB	�qB	��B	��B	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	�B	�)B	�5B	�5B	�;B	�;B	�BB	�BB	�HB	�ZB	�`B	�`B	�`B	�`B	�fB	�B	�B	��B	��B
  B
B
B
+B

=B
hB
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
$�B
%�B
&�B
(�B
(�B
)�B
+B
,B
.B
/B
2-B
5?B
6FB
9XB
=qB
=qB
>wB
?}B
?}B
@�B
@�B
A�B
C�B
D�B
E�B
F�B
I�B
K�B
L�B
O�B
R�B
S�B
T�B
S�B
S�B
T�B
VB
VB
W
B
YB
ZB
[#B
_;B
aHB
aHB
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
dZB
e`B
ffB
hsB
k�B
n�B
o�B
q�B
s�B
t�B
w�B
w�B
w�B
y�B
z�B
z�B
z�B
{�B
~�B
~�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�7B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�=B
�DB
�VB
�\B
�VB
�\B
�hB
�oB
�uB
�{B
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
��B
��B
��B
��B
��B
�B
�B
�B
�!B
�9B
�RB
�RB
�XB
�XB
�^B
�XB
�XB
�^B
�XB
�XB
�XB
�^B
�dB
�dB
�dB
�dB
�jB
�jB
�qB
�wB
�}B
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
��B
ÖB
ĜB
ŢB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ǮB
ǮB
ǮB
ȴB
ɺB
ɺB
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
��B
��B
��B
��B
��B
��B
�B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�#B
�#B
�)B
�)B
�)B
�/B
�/B
�/B
�;B
�BB
�BB
�HB
�HB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�`B
�`B
�`B
�`B
�fB
�fB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�sB
�sB
�B
�yB
�yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
��B
��B
��B
��B
��B  B  B  B  B  B  B  B  B  BBBBBBBBBBBBBB%B%B%B%B+B+B+B1B1B	7B	7B	7B	7B	7B	7B	7B	7B	7B	7B
=B
=B
=B
=BDBDBDBJBJBPBPBPBPBPBPBPBPBVBVBVBVBVBVB\B\B\B\BbBbBbBbBbBbBbBbBbBbBhBhBhBhBhBhBoBuBuB{B{B{B{B{B{B{B{B{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B �B �B �B �B!�B!�B!�B!�B!�B!�B!�B"�B"�B"�B"�B"�B"�B#�B#�B#�B#�B#�B#�B#�B$�B$�B$�B%�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B�B�B�B�B�B�!B�'B�-B�-B�3B�3B�?B�FB�LB�XB�dB�qB�}B��BBŢBǮBɺB��B��B��B��B��B��B�B�/B�fB�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B�B�B�B�B�ZB�5B�B��BƨBɺBǮBĜB�}B�RB��B�bB�DB�Bx�Bm�BffB`ABW
BI�B?}B6FB2-B/B+B(�B%�B�B1B��B��B��B�B�yB�NB��B�FB��B��B��B��B�hB�Bq�BP�B=qB2-B0!B.B,B+B(�B%�B!�B�BB
��B
��B
�yB
�/B
��B
��B
�?B
��B
�+B
x�B
n�B
ZB
N�B
J�B
F�B
B�B
7LB
0!B
.B
)�B
"�B
�B
oB
PB
B	��B	�B	�B	�B	�B	�yB	�TB	��B	��B	�FB	��B	�oB	�=B	�B	�B	z�B	w�B	q�B	l�B	iyB	gmB	e`B	aHB	]/B	YB	XB	T�B	Q�B	P�B	S�B	VB	YB	YB	ZB	YB	VB	M�B	G�B	D�B	D�B	C�B	C�B	C�B	C�B	B�B	A�B	@�B	>wB	;dB	7LB	5?B	2-B	.B	%�B	�B	�B	�B	�B	�B	�B	oB	VB	JB	
=B	+B	B	B	B	B	  B��B��B��B�B�B�B�B�B�sB�ZB�AB�;B�5B�/B�)B�#B�B�B�B�
B�B��B��B��B��B��B��B��B��B��B��B��B��B��BɺBɺBǮBǮBǮBǮBƨBŢBĜBƨBƨBǮBǮBȴBǮBƨBĜBÕBŢBƨBŢBƨBŢBĜBĜBĜBĜBĜBŢBƨBƨBŢBƨBȴB��B��B��B��B��B��B��B��B��B��B�
B�)B�/B�HB�ZB�ZB�fB�sB�mB�mB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	B	  B��B	B	B	%B		7B		7B		7B	1B		7B	VB	bB	hB	oB	uB	{B	{B	{B	�B	�B	�B	�B	�B	�B	!�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	&�B	'�B	(�B	)�B	)�B	+B	-B	.B	0!B	6FB	7LB	9XB	<jB	@�B	A�B	B�B	C�B	I�B	O�B	T�B	T�B	YB	[#B	[#B	aHB	dZB	e`B	jB	k�B	l�B	l�B	l�B	l�B	l�B	l�B	l�B	n�B	o�B	q�B	{�B	� B	�B	�B	�B	�B	�B	�B	�B	�1B	�7B	�7B	�7B	�7B	�=B	�\B	��B	��B	��B	��B	��B	��B	�B	�B	�?B	�dB	�jB	�qB	�qB	�wB	��B	B	ĜB	ĜB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�/B	�HB	�HB	�NB	�TB	�TB	�ZB	�ZB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
%B
%B
%B
%B
%B
+B
+B
+B
+B
+B
1B
	7B

=B
JB
\B
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
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
%�B
%�B
&�B
&�B
'�B
(�B
(�B
)�B
+B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
+B
,B
,B
-B
.B
/B
/B
/B
/B
/B
/B
/B
.B
/B
2-B
33B
2-B
33B
5?B
6FB
7LB
8RB
9XB
:^B
;dB
<jB
=qB
@�B
A�B
B�B
C�B
C�B
D�B
E�B
F�B
H�B
I�B
L�B
P�B
R�B
R�B
S�B
XB
\)B
\)B
]/B
]/B
^5B
]/B
]/B
^5B
]/B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
`AB
`AB
aHB
bNB
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
gmB
hsB
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
l�B
m�B
m�B
n�B
n�B
n�B
o�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
x�B
x�B
y�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
}�B
}�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�7B
�7B
�7B
�7B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�VB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�bB
�bB
�bB
�bB
�hB
�hB
�oB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
�{B
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
��B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�-B
�-B
�-B
�-B
�-B
�-B
�3B
�3B
�3B
�3B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�?B
�?B
�?B
�?B
�?B
�?B
�FB
�LB
�LB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�XB
�^B
�^B
�dB
�dB
�dB
�dB
�dB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�qB
�qB
�qB
�qB
�qB
�wB
�wB
�wB
�}B
�}B
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
B
B
B
ÕB
ÕB
ÕB
ÕB
ÕB
ÕB
ÕB
ÕB
ÕB
ÕB
ÕB
ÕB
ÕB
ÕB
ÕB
ÕB
ÕB
ÕB
ÕB
ÕB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ȴB
ȴB
ȴB
ɺ4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                 ! ! !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ' \0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000PRES            TEMP            PSAL            NB_SAMPLE_CTD   PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            surface_pressure=-0.15 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     salinity_offset = -0.0900000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                       PSAL ADJUST [dd mm yyyy N S_off stddev] 05 10 2018 117 -0.0900000 0.0040 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                        20230608223112              20230608223112              AO  ARCAADJP                                                                    20230608223112    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20230608223112    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230608223112  QCP$                G�O�G�O�G�O�1B83E           AO  ARGQQCPL                                                                    20230608223112  QCF$                G�O�G�O�G�O�8000            