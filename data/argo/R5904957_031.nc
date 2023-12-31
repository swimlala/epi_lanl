CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:10Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  A�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  J8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  P�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  W�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  YP   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  `   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  a�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  hh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140810  20181024140810  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׷d����1   @׷e)�,@3F$�/��c��G�{1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   B   B   @333@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B���C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB�CD�CF�CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C��3D � D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D��D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'� D(  D(� D(��D)y�D*  D*� D+  D+� D,  D,� D-  D-� D.  D.�fD/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8�fD9fD9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� D@��DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJy�DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DTy�DU  DU� DV  DV�fDW  DW� DX  DXy�DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D]��D^� D_fD_� D`  D`� Da  Day�Da��Dby�Db��Dc� DdfDd�fDe  Dey�Df  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� DkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�fDy�qD�D�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @<��@���@���AffA"ffABffAbffA�33A�33A�33A�33A�33A�33A�33A�33B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B��B�L�B�L�B��B�L�B��B�L�BЀ B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B��C*&fC,&fC.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@@ CB@ CD@ CF@ CH&fCJ�CL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb�Cd�Cf&fCh&fCj&fCl&fCn&fCp&fCr&fCt&fCv&fCx&fCz&fC|@ C~&fC�3C�3C�3C�3C�  C�3C�3C�3C�3C�fC�3C�3C�3C�3C�3C�3C�3C�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�fC�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�fC�3C�3C�3C�  C�3C�fC�fC�3C�3C�3C�3C�3C�fC�3C�3D 3D ��D	�D��D	�D��D4D��D	�D��D	�D��D	�D��D	�D��D	�D��D		�D	��D
	�D
��D	�D��D4D��D	�D��D	�D��D	�D��D	�D��D4D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D� D	�D��D	�D��D	�D��D D��D 	�D ��D!	�D!��D"	�D"��D#	�D#��D$	�D$��D%	�D%� D&	�D&��D'	�D'��D(	�D(��D)4D)�4D*	�D*��D+	�D+��D,	�D,��D-	�D-��D.	�D.� D/	�D/��D0	�D0��D1	�D1� D2	�D2��D3	�D3��D4	�D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8� D9 D9��D:	�D:��D;	�D;��D<	�D<��D=	�D=��D>	�D>��D?	�D?��D@	�D@��DA4DA��DB	�DB��DC	�DC��DD	�DD��DE	�DE��DF	�DF��DG	�DG��DH	�DH��DI	�DI��DJ	�DJ�4DK	�DK��DL	�DL��DM DM��DN	�DN��DO	�DO��DP	�DP��DQ	�DQ��DR	�DR��DS	�DS��DT	�DT�4DU	�DU��DV	�DV� DW	�DW��DX	�DX�4DY	�DY��DZ	�DZ��D[	�D[��D\	�D\��D]	�D]�4D^4D^��D_ D_��D`	�D`��Da	�Da�4Db4Db�4Dc4Dc��Dd Dd� De	�De�4Df	�Df� Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj��Dk Dk��Dl	�Dl��Dm	�Dm��Dn	�Dn��Do	�Do��Dp	�Dp��Dq	�Dq��Dr	�Dr��Ds	�Ds��Dt4Dt��Du	�Du��Dv	�Dv��Dw	�Dw��Dw� Dy�D�I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A���A���A���A���Aղ-AլAՓuA�n�A�;dA�+A�VA��HAԏ\A�  AӍPA�(�A���A�|�A��A�bA�n�A��A���A��TAυA͝�A��A˾wA�ƨA���A��AǮA�I�A�A�A�33A�ƨAę�A�jA�`BA�K�A�1'A�`BAA��A�hsA���A�dZA�+A�"�A�7LA�7LA��!A�1A��A�v�A��mA��RA���A�A��hA���A���A�C�A��/A��/A���A�1'A��TA��+A�t�A��`A���A��wA�oA�r�A���A�hsA�~�A��
A�t�A�ZA�^5A���A��TA~5?Az�yAy7LAw��Av�/Au��As�Ar�yAq�wAo+Ak��AjE�AhJAgdZAc�^AaVA_��A^��A]dZA[�hAX�AV�AR�API�AMALE�AJbNAI/AG�
AEACK�AB��AB�+ABI�AB$�AA��A@z�A?�FA?G�A>�/A<��A9�-A8JA6�yA3�FA1hsA1oA/�7A/oA.�A-oA+|�A*�A*��A)�PA%�;A$�\A#ƨA#p�A"�yA��A
�!A	K�A~�A|�A^5A�A�jA5?A�
AAA�A33A 5?@�
=@�~�@�ff@��-@�+@�J@���@�?}@�"�@�j@�A�@�(�@�b@�bN@ۮ@ڏ\@�J@�hs@�V@�1'@׍P@��@�n�@Չ7@Լj@� �@�+@�V@��#@љ�@�hs@���@�I�@�\)@·+@�p�@�I�@ˮ@�S�@�\)@�S�@�C�@�+@�+@�S�@˥�@� �@ˍP@ʏ\@�hs@�r�@�A�@Ǖ�@�o@Ƈ+@�@��@��#@�@ũ�@��/@Õ�@��@���@�V@�J@��@�V@�"�@���@�ff@�E�@�@���@�?}@��@���@���@��u@��@�ȴ@�V@�J@��`@��`@���@��\@���@�ȴ@�%@��/@���@���@�n�@�$�@��@���@���@�?}@���@�A�@��m@�S�@��@���@���@���@���@�n�@�J@��@���@�
=@��#@�Q�@�@�{@�O�@���@���@��u@�r�@�Q�@�  @��@���@�b@���@�Q�@�{@�x�@��@���@�V@���@��@�-@�S�@���@�Z@�b@���@��m@�I�@� �@��;@�l�@�
=@���@�v�@��T@�O�@��@�b@�|�@���@�~�@�E�@�-@�{@��@�@�/@�z�@�1'@��@��@�;d@�o@���@�@��@��h@�?}@��@�z�@�1'@��@�b@���@��P@�;d@���@�ȴ@��\@��@��@��^@�?}@��/@��9@��u@��@�bN@�I�@��w@�K�@�"�@��@�V@�=q@�J@��#@���@�hs@�?}@�?}@�?}@�?}@�7L@�7L@�7L@�7L@�/@�&�@��@��`@��`@���@���@� �@���@�|�@�"�@�o@�o@�ȴ@�E�@��@��^@��h@�?}@��@�A�@���@��m@��@�\)@�C�@��@���@�V@�5?@��@��T@��-@��7@�?}@�%@��`@���@��9@���@��D@�Q�@�(�@�ƨ@�K�@�C�@�C�@�"�@���@��!@���@nl�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A���A���A���A���Aղ-AլAՓuA�n�A�;dA�+A�VA��HAԏ\A�  AӍPA�(�A���A�|�A��A�bA�n�A��A���A��TAυA͝�A��A˾wA�ƨA���A��AǮA�I�A�A�A�33A�ƨAę�A�jA�`BA�K�A�1'A�`BAA��A�hsA���A�dZA�+A�"�A�7LA�7LA��!A�1A��A�v�A��mA��RA���A�A��hA���A���A�C�A��/A��/A���A�1'A��TA��+A�t�A��`A���A��wA�oA�r�A���A�hsA�~�A��
A�t�A�ZA�^5A���A��TA~5?Az�yAy7LAw��Av�/Au��As�Ar�yAq�wAo+Ak��AjE�AhJAgdZAc�^AaVA_��A^��A]dZA[�hAX�AV�AR�API�AMALE�AJbNAI/AG�
AEACK�AB��AB�+ABI�AB$�AA��A@z�A?�FA?G�A>�/A<��A9�-A8JA6�yA3�FA1hsA1oA/�7A/oA.�A-oA+|�A*�A*��A)�PA%�;A$�\A#ƨA#p�A"�yA��A
�!A	K�A~�A|�A^5A�A�jA5?A�
AAA�A33A 5?@�
=@�~�@�ff@��-@�+@�J@���@�?}@�"�@�j@�A�@�(�@�b@�bN@ۮ@ڏ\@�J@�hs@�V@�1'@׍P@��@�n�@Չ7@Լj@� �@�+@�V@��#@љ�@�hs@���@�I�@�\)@·+@�p�@�I�@ˮ@�S�@�\)@�S�@�C�@�+@�+@�S�@˥�@� �@ˍP@ʏ\@�hs@�r�@�A�@Ǖ�@�o@Ƈ+@�@��@��#@�@ũ�@��/@Õ�@��@���@�V@�J@��@�V@�"�@���@�ff@�E�@�@���@�?}@��@���@���@��u@��@�ȴ@�V@�J@��`@��`@���@��\@���@�ȴ@�%@��/@���@���@�n�@�$�@��@���@���@�?}@���@�A�@��m@�S�@��@���@���@���@���@�n�@�J@��@���@�
=@��#@�Q�@�@�{@�O�@���@���@��u@�r�@�Q�@�  @��@���@�b@���@�Q�@�{@�x�@��@���@�V@���@��@�-@�S�@���@�Z@�b@���@��m@�I�@� �@��;@�l�@�
=@���@�v�@��T@�O�@��@�b@�|�@���@�~�@�E�@�-@�{@��@�@�/@�z�@�1'@��@��@�;d@�o@���@�@��@��h@�?}@��@�z�@�1'@��@�b@���@��P@�;d@���@�ȴ@��\@��@��@��^@�?}@��/@��9@��u@��@�bN@�I�@��w@�K�@�"�@��@�V@�=q@�J@��#@���@�hs@�?}@�?}@�?}@�?}@�7L@�7L@�7L@�7L@�/@�&�@��@��`@��`@���@���@� �@���@�|�@�"�@�o@�o@�ȴ@�E�@��@��^@��h@�?}@��@�A�@���@��m@��@�\)@�C�@��@���@�V@�5?@��@��T@��-@��7@�?}@�%@��`@���@��9@���@��D@�Q�@�(�@�ƨ@�K�@�C�@�C�@�"�@���@��!@���@nl�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BbBbBbBbBbBbBbBbBhBhBuBuB�B�B#�B#�B$�B)�B1'B6FBH�BO�BVB[#BbNBp�Bq�Bu�B|�B�B�Bu�Bw�B�7B��B�B�BÖB�B��BPB#�B,B-B,B,B,B=qBP�BO�BXBZBS�BaHBhsBr�B{�B�=B��B��BȴBZBN�BB�B=qB-B%B�B��B�'B�1B��B�=Bs�BZBC�B9XB!�B
�B
�B
�BB
�B
��B
�wB
�uB
v�B
R�B
@�B
(�B
�B
1B	�B	�sB	�BB	�B	��B	ȴB	��B	�RB	�B	��B	�hB	�B	|�B	m�B	^5B	W
B	P�B	H�B	>wB	33B	&�B	�B	JB��B��B�B�`B�;B�
B��B��B��B��B��BɺBĜBB�}B�jB�FB�!B�B��B��B��B��B��B��B��B��B��B�{B�hB�VB�=B�JB�=B�7B�DA�%B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B
XB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�9B�XB�qB�wB�wBBÖBÖBɺB��B��B�B�/B�HB�`B�`B�mB�yB�B�B�B�B�B�B�B��B��B��B��B��B	B	B	VB	bB	hB	oB	uB	{B	�B	�B	�B	�B	�B	�B	$�B	&�B	&�B	)�B	.B	0!B	0!B	49B	5?B	/B	#�B	&�B	'�B	+B	,B	.B	/B	0!B	2-B	5?B	9XB	;dB	@�B	B�B	D�B	H�B	O�B	S�B	S�B	S�B	XB	bNB	iyB	gmB	dZB	bNB	dZB	gmB	k�B	n�B	o�B	q�B	r�B	t�B	w�B	{�B	�B	�=B	�=B	�B	�B	�+B	�1B	�JB	�PB	�bB	��B	��B	��B	��B	��B	��B	�B	�?B	�dB	�qB	��B	B	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�)B	�/B	�/B	�;B	�;B	�BB	�HB	�TB	�TB	�ZB	�`B	�fB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
+B

=B
DB
DB
JB
JB
PB
PB
PB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
oB
oB
uB
uB
uB
�B
�B
-)111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BbBbBbBbBbBbBbBbBhBhBuBuB�B�B#�B#�B$�B)�B1'B6FBH�BO�BVB[#BbNBp�Bq�Bu�B|�B�B�Bu�Bw�B�7B��B�B�BÖB�B��BPB#�B,B-B,B,B,B=qBP�BO�BXBZBS�BaHBhsBr�B{�B�=B��B��BȴBZBN�BB�B=qB-B%B�B��B�'B�1B��B�=Bs�BZBC�B9XB!�B
�B
�B
�BB
�B
��B
�wB
�uB
v�B
R�B
@�B
(�B
�B
1B	�B	�sB	�BB	�B	��B	ȴB	��B	�RB	�B	��B	�hB	�B	|�B	m�B	^5B	W
B	P�B	H�B	>wB	33B	&�B	�B	JB��B��B�B�`B�;B�
B��B��B��B��B��BɺBĜBB�}B�jB�FB�!B�B��B��B��B��B��B��B��B��B��B�{B�hB�VB�=B�JB�=B�7B�DA�%B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B
XB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�9B�XB�qB�wB�wBBÖBÖBɺB��B��B�B�/B�HB�`B�`B�mB�yB�B�B�B�B�B�B�B��B��B��B��B��B	B	B	VB	bB	hB	oB	uB	{B	�B	�B	�B	�B	�B	�B	$�B	&�B	&�B	)�B	.B	0!B	0!B	49B	5?B	/B	#�B	&�B	'�B	+B	,B	.B	/B	0!B	2-B	5?B	9XB	;dB	@�B	B�B	D�B	H�B	O�B	S�B	S�B	S�B	XB	bNB	iyB	gmB	dZB	bNB	dZB	gmB	k�B	n�B	o�B	q�B	r�B	t�B	w�B	{�B	�B	�=B	�=B	�B	�B	�+B	�1B	�JB	�PB	�bB	��B	��B	��B	��B	��B	��B	�B	�?B	�dB	�qB	��B	B	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�)B	�/B	�/B	�;B	�;B	�BB	�HB	�TB	�TB	�ZB	�`B	�fB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
+B

=B
DB
DB
JB
JB
PB
PB
PB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
oB
oB
uB
uB
uB
�B
�B
-)111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.15 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140810                              AO  ARCAADJP                                                                    20181024140810    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140810  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140810  QCF$                G�O�G�O�G�O�4000            