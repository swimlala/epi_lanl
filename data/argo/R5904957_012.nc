CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:05Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140805  20181024140805  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @ץ�A���1   @ץ���Č@4C�
=p��c���vȴ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�33B   BffB  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bg��Bo��Bx  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$�C&�C(�C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��3C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  Dy�D  D�fDfD� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&y�D&��D'� D(  D(� D(��D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@fD@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DK��DLy�DM  DM� DN  DN� DOfDO� DP  DPy�DQ  DQ� DR  DRy�DR��DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[�fD\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dp��Dq� Dr  Dr� Ds  Ds� DtfDt� Du  Du� DvfDv� Dw  Dwy�Dw� Dy��D�,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���AffA"ffABffAbffA�33A�33A�33A�33A�33A�33A�33A�ffB ��B	  B��B��B ��B(��B0��B8��B@��BI  BP��BX��B`��Bh34Bp34Bx��B�L�B�L�B�L�B�L�B�L�B�L�B��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B��B�L�B�L�B�L�B�L�B��B��B�L�B�L�B�L�B�L�B� B�L�B�L�B�L�B�L�B�L�C &fC&fC&fC&fC&fC
&fC&fC&fC@ C&fC&fC&fC&fC&fC&fC&fC &fC"&fC$@ C&@ C(@ C*&fC,�C.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^�C`�Cb&fCd&fCf&fCh&fCj&fCl&fCn&fCp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�fC�3C�3C�3C�3C�fC�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�fC�3C�3C�3C�3C�3C�3C�3C�3C�fC�3C�3C�3C�3C�3C�  C�  C�3C�3C�3C�3C�fC�3C�3C�3C�3C�  C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�fC�3C�  C�3C�3C�fC�3C�3C�3C�  C�  C�3C�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D ��D	�D��D	�D��D	�D��D	�D��D	�D��D4D�4D	�D��D	�D��D		�D	��D
	�D
��D	�D��D	�D��D	�D�4D	�D� D D��D	�D��D4D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D� D	�D��D 	�D ��D!	�D!��D"	�D"��D#	�D#��D$	�D$��D%	�D%�4D&	�D&�4D'4D'��D(	�D(��D)4D)��D*	�D*��D+	�D+��D,	�D,��D-	�D-��D.	�D.��D/	�D/��D0	�D0��D1	�D1� D2	�D2�4D3	�D3��D4	�D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8��D9	�D9��D:	�D:��D;	�D;��D<	�D<��D=	�D=��D>	�D>��D?	�D?��D@ D@��DA	�DA��DB	�DB��DC	�DC��DD	�DD��DE	�DE��DF	�DF��DG	�DG��DH	�DH��DI	�DI��DJ	�DJ��DK DK��DL4DL�4DM	�DM��DN	�DN��DO DO��DP	�DP�4DQ	�DQ��DR	�DR�4DS4DS��DT	�DT��DU	�DU��DV	�DV��DW	�DW� DX	�DX��DY	�DY��DZ	�DZ��D[	�D[� D\ D\��D]	�D]��D^	�D^��D_	�D_��D`	�D`��Da	�Da��Db	�Db��Dc	�Dc��Dd	�Dd��De	�De��Df	�Df��Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj��Dk	�Dk��Dl	�Dl��Dm	�Dm��Dn	�Dn��Do	�Do��Dp	�Dp�4Dq4Dq��Dr	�Dr��Ds	�Ds��Dt Dt��Du	�Du��Dv Dv��Dw	�Dw�4Dw�Dy�]D�1�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�JA�bA�bA�{A��A�{A��A��A��A��A��A��A� �A��A� �A� �A�"�A�"�A� �AʍPA��A�I�A�%A��mAǾwAǟ�AǏ\A�hsA�-A�$�A�bAƴ9AŇ+Aĝ�A�C�A��/A£�A�Q�A��A��yA�VA��A��A�-A�ĜA��jA��A���A�;dA��
A��+A�/A��A���A�/A��A�K�A�?}A��;A��;A�-A��FA��mA��uA��mA�XA��wA��A�
=A�(�A�-A�Q�A��7A�+A��!A���A��HA�l�A�oA��\A���A��
A��!A�jA��+A���A�C�A�$�A��A�bA�33A�{A��A�p�A�t�A�hsA��7A�n�A���A���A��A���A��A�XA�?}A���A��HA���A~{Av �AohsAm��Ak�mAj(�AgƨAe��Ad��AdQ�Aa`BA`ȴA`ffA`=qA^�RA\=qAZ��AU�hATAR�AL��AI?}AH9XAHAG��AE�AB��AB  AA|�A@=qA=�-A;C�A;�A9�mA9�A8M�A6�jA3p�A29XA0r�A/\)A.ȴA.r�A-A,-A+VA)&�A'�A&�/A%�TA%O�A$�+A#�A#A!XA 1A%A�+A��A�/A�AĜAZAM�A5?A�A��A=qA^5A7LA�A�9AM�A�mA��AhsA�A��A�DAz�Ar�AffAZAM�A=qA9XA�A?}A��A�A
�uA
-A
1A	�A	K�A	&�A	
=A�A��AE�AoA��A �uA  �@�n�@�I�@��m@�$�@�X@�&�@�"�@���@�@�J@�7L@�?}@��#@�j@�dZ@�v�@�7@�j@��H@�`B@�D@�A�@�K�@�E�@���@�h@���@��@�;d@�33@��H@��;@�J@ە�@�=q@���@�I�@�?}@�l�@�~�@���@�33@·+@�J@�@͑h@�/@�I�@˕�@�l�@�+@��@�@��y@��H@���@ʸR@�~�@���@�j@�|�@�ȴ@�J@��/@öF@�|�@�33@��H@�=q@���@���@�I�@���@��P@�dZ@�;d@�33@��@���@�E�@��@��@��@�A�@�C�@��+@�@�$�@�=q@�@���@���@�M�@�O�@��u@�9X@��@���@��F@���@�"�@�n�@���@�O�@��j@���@�C�@�ȴ@���@�33@��
@��@���@��H@���@�5?@�$�@���@���@��7@�G�@��9@�j@�bN@�Q�@�b@���@�
=@���@�~�@�$�@��T@���@�`B@�V@���@��@��D@�bN@��
@���@��!@�{@�p�@���@��@��@��;@��P@�+@�o@��@���@��R@��R@��!@�~�@��@��@�Q�@���@�+@�~�@�{@���@���@��@�?}@��`@���@�bN@�1'@��F@���@���@���@���@���@���@���@���@���@���@���@�|�@�S�@���@�%@��9@�A�@�l�@��H@���@���@�5?@��h@�Ĝ@���@�r�@� �@���@�t�@�;d@��@�ȴ@��!@��+@�J@���@��`@��@��/@�Ĝ@�A�@��@���@�C�@�
=@�
=@��H@���@�^5@��-@��/@�z�@�bN@�Z@�A�@�(�@�  @��
@�ƨ@��F@��@���@���@���@���@��P@�|�@�C�@�+@���@�-@�$�@��@�@���@�`B@�&�@�V@��j@���@�z�@�bN@�Q�@�I�@�1'@��@��m@���@��F@��@��@�K�@��@��@�x�@�?}@��/@�z�@�I�@�1@�ƨ@��@�S�@���@�$�@��T@��h@�x�@�p�@�O�@o�Q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�JA�bA�bA�{A��A�{A��A��A��A��A��A��A� �A��A� �A� �A�"�A�"�A� �AʍPA��A�I�A�%A��mAǾwAǟ�AǏ\A�hsA�-A�$�A�bAƴ9AŇ+Aĝ�A�C�A��/A£�A�Q�A��A��yA�VA��A��A�-A�ĜA��jA��A���A�;dA��
A��+A�/A��A���A�/A��A�K�A�?}A��;A��;A�-A��FA��mA��uA��mA�XA��wA��A�
=A�(�A�-A�Q�A��7A�+A��!A���A��HA�l�A�oA��\A���A��
A��!A�jA��+A���A�C�A�$�A��A�bA�33A�{A��A�p�A�t�A�hsA��7A�n�A���A���A��A���A��A�XA�?}A���A��HA���A~{Av �AohsAm��Ak�mAj(�AgƨAe��Ad��AdQ�Aa`BA`ȴA`ffA`=qA^�RA\=qAZ��AU�hATAR�AL��AI?}AH9XAHAG��AE�AB��AB  AA|�A@=qA=�-A;C�A;�A9�mA9�A8M�A6�jA3p�A29XA0r�A/\)A.ȴA.r�A-A,-A+VA)&�A'�A&�/A%�TA%O�A$�+A#�A#A!XA 1A%A�+A��A�/A�AĜAZAM�A5?A�A��A=qA^5A7LA�A�9AM�A�mA��AhsA�A��A�DAz�Ar�AffAZAM�A=qA9XA�A?}A��A�A
�uA
-A
1A	�A	K�A	&�A	
=A�A��AE�AoA��A �uA  �@�n�@�I�@��m@�$�@�X@�&�@�"�@���@�@�J@�7L@�?}@��#@�j@�dZ@�v�@�7@�j@��H@�`B@�D@�A�@�K�@�E�@���@�h@���@��@�;d@�33@��H@��;@�J@ە�@�=q@���@�I�@�?}@�l�@�~�@���@�33@·+@�J@�@͑h@�/@�I�@˕�@�l�@�+@��@�@��y@��H@���@ʸR@�~�@���@�j@�|�@�ȴ@�J@��/@öF@�|�@�33@��H@�=q@���@���@�I�@���@��P@�dZ@�;d@�33@��@���@�E�@��@��@��@�A�@�C�@��+@�@�$�@�=q@�@���@���@�M�@�O�@��u@�9X@��@���@��F@���@�"�@�n�@���@�O�@��j@���@�C�@�ȴ@���@�33@��
@��@���@��H@���@�5?@�$�@���@���@��7@�G�@��9@�j@�bN@�Q�@�b@���@�
=@���@�~�@�$�@��T@���@�`B@�V@���@��@��D@�bN@��
@���@��!@�{@�p�@���@��@��@��;@��P@�+@�o@��@���@��R@��R@��!@�~�@��@��@�Q�@���@�+@�~�@�{@���@���@��@�?}@��`@���@�bN@�1'@��F@���@���@���@���@���@���@���@���@���@���@���@�|�@�S�@���@�%@��9@�A�@�l�@��H@���@���@�5?@��h@�Ĝ@���@�r�@� �@���@�t�@�;d@��@�ȴ@��!@��+@�J@���@��`@��@��/@�Ĝ@�A�@��@���@�C�@�
=@�
=@��H@���@�^5@��-@��/@�z�@�bN@�Z@�A�@�(�@�  @��
@�ƨ@��F@��@���@���@���@���@��P@�|�@�C�@�+@���@�-@�$�@��@�@���@�`B@�&�@�V@��j@���@�z�@�bN@�Q�@�I�@�1'@��@��m@���@��F@��@��@�K�@��@��@�x�@�?}@��/@�z�@�I�@�1@�ƨ@��@�S�@���@�$�@��T@��h@�x�@�p�@�O�@o�Q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
�BBF�BiyBv�B|�B�B�B�B�DB��B�?BĜB��B��B��BȴB��B��B%BhB�B(�BA�BI�BO�BXBr�B�B�7B�=B�7B�1B�+B�B�B�B�B�B� Bv�Bp�Bm�BhsB]/BO�BH�BE�BC�B>wB9XB33B-B"�B�B�B�BuBVBDB1BB��B�sB�BɺB��B��B�BaHBN�B1'B+B
��B
�ZB
ƨB
�?B
��B
�oB
�B
p�B
`BB
Q�B
D�B
=qB
;dB
:^B
1'B
&�B
�B	��B	��B	�VB	�B	q�B	e`B	T�B	H�B	?}B	<jB	'�B	$�B	!�B	�B	{B	B��B�;B��BɺB�3B��B��B��B��B��B�oB�bB�VB�DB�=B�B�B�B� B|�Bx�B}�B�B}�B}�B{�Bz�B}�B|�B}�B�B�B�B�+B�DB�JB�PB�\B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�3B�?B�?B�9B�3B�3B�3B�3B�?B�3B�3B�3B�3B�-B�-B�'B�B�B�B�B�!B�B�B�!B�'B�'B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B�B�9B�FB�?B�9B�-B�-B�-B�3B�-B�'B�!B�B�B�B�B�B�B�B�B�!B�'B�9B�FB�FB�FB�LB�qB�}BĜB��B��B��B��B��B��B��B�
B�
B�B�B�B�B�B�B�/B�;B�TB�B�B�B�B�B��B��B��B��B	  B	B	B	B	B	B	B	B	B	B	B	%B		7B	
=B	JB	JB	hB	{B	�B	�B	 �B	'�B	&�B	%�B	'�B	)�B	,B	.B	/B	/B	0!B	1'B	33B	5?B	7LB	9XB	<jB	@�B	D�B	F�B	K�B	P�B	XB	[#B	]/B	`BB	aHB	dZB	ffB	hsB	jB	l�B	r�B	y�B	{�B	{�B	{�B	}�B	�B	�B	�%B	�+B	�7B	�7B	�7B	�=B	�DB	�PB	�PB	�VB	�VB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�?B	�^B	�dB	�wB	��B	��B	B	B	ÖB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�;B	�;B	�HB	�TB	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
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
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
	7B
DB
PB
PB
VB
bB
hB
oB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
'�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
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
�BBF�BiyBv�B|�B�B�B�B�DB��B�?BĜB��B��B��BȴB��B��B%BhB�B(�BA�BI�BO�BXBr�B�B�7B�=B�7B�1B�+B�B�B�B�B�B� Bv�Bp�Bm�BhsB]/BO�BH�BE�BC�B>wB9XB33B-B"�B�B�B�BuBVBDB1BB��B�sB�BɺB��B��B�BaHBN�B1'B+B
��B
�ZB
ƨB
�?B
��B
�oB
�B
p�B
`BB
Q�B
D�B
=qB
;dB
:^B
1'B
&�B
�B	��B	��B	�VB	�B	q�B	e`B	T�B	H�B	?}B	<jB	'�B	$�B	!�B	�B	{B	B��B�;B��BɺB�3B��B��B��B��B��B�oB�bB�VB�DB�=B�B�B�B� B|�Bx�B}�B�B}�B}�B{�Bz�B}�B|�B}�B�B�B�B�+B�DB�JB�PB�\B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�3B�?B�?B�9B�3B�3B�3B�3B�?B�3B�3B�3B�3B�-B�-B�'B�B�B�B�B�!B�B�B�!B�'B�'B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B�B�9B�FB�?B�9B�-B�-B�-B�3B�-B�'B�!B�B�B�B�B�B�B�B�B�!B�'B�9B�FB�FB�FB�LB�qB�}BĜB��B��B��B��B��B��B��B�
B�
B�B�B�B�B�B�B�/B�;B�TB�B�B�B�B�B��B��B��B��B	  B	B	B	B	B	B	B	B	B	B	B	%B		7B	
=B	JB	JB	hB	{B	�B	�B	 �B	'�B	&�B	%�B	'�B	)�B	,B	.B	/B	/B	0!B	1'B	33B	5?B	7LB	9XB	<jB	@�B	D�B	F�B	K�B	P�B	XB	[#B	]/B	`BB	aHB	dZB	ffB	hsB	jB	l�B	r�B	y�B	{�B	{�B	{�B	}�B	�B	�B	�%B	�+B	�7B	�7B	�7B	�=B	�DB	�PB	�PB	�VB	�VB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�?B	�^B	�dB	�wB	��B	��B	B	B	ÖB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�;B	�;B	�HB	�TB	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
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
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
	7B
DB
PB
PB
VB
bB
hB
oB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
'�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.15 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140805                              AO  ARCAADJP                                                                    20181024140805    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140805  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140805  QCF$                G�O�G�O�G�O�0               