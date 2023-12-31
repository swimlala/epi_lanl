CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:50Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  mP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191650  20181005191650  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               	A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @ף\��1   @ף�s�r@3/��w�c�z�G�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      	A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�fC  C  C  C   C"  C$  C&  C'�fC)�fC,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCS�fCV  CX  CZ  C\  C^  C`�Cb  Cd  Ce�fCg�fCj  Cl  Cn  Cp�Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C��3C��C��C�  C��3C��3C�  C��C��C��3C�  C�  C��3C��3C�  C��C��3C��3C�  C�  C�  C��3C��3C�  C�  C��3C�  C��C��3C��3C��3C�  C��C�  C�  C�  C�  C��C��C�  C��3C��3C�  C��C��C��3C�  C��C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C��3C�  C��C�  C��3C��C�  C��3C�  C��3C�  C��C��C��C��C�  C�  C��3C�  C��C��C�  C��3C��3C��fC��3C��3C��3C��3C�  C��C��3C��fC��3C��3C�  C�  C��3C��3C��3C�  C�  C��C�  C�  C��C�  C�  C��C��3C�  C�  C��3C�  C�  C�  C�  C�  D   D �fDfD� D  D� D��Dy�D  D� DfD� D��D� D  Dy�D  D� D	  D	y�D
fD
y�DfD� DfD� D��D� D��D�fD��D�fD��D� D��D�fD��D� D  D�fDfD� D  D� D  D�fD��D� D  Dy�D  D�fD  D� DfD�fD��D� D  D)  D)� D*  D*� D+  D+� D,  D,� D,��D-y�D-��D.y�D/fD/��D0fD0y�D1fD1� D1��D2�fD3  D3y�D4fD4y�D5fD5� D6fD6� D7fD7� D8fD8� D9  D9y�D:  D:� D;  D;�fD<fD<�fD=  D=y�D>fD>y�D?  D?�fD@  D@� DA  DAs3DB  DB�fDCfDC� DDfDD� DE  DE� DFfDF� DF��DG� DH  DHy�DH��DI� DJfDJ�fDK  DK�fDL  DL� DM  DM� DN  DNy�DOfDO�fDP  DP� DQ  DQy�DR  DR� DS  DS� DS��DTy�DT�3DU� DV  DV� DW  DWy�DX  DX�fDY  DY� DZ  DZ� D[  D[�fD\fD\� D]  D]� D]��D^s3D^��D_�fD`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe�fDf  Dfy�Dg  Dg�fDh  Dh� Di  Diy�Dj  Dj�fDkfDk�fDlfDl��Dm  Dmy�DnfDn� Do  Do�fDp  Dp� DqfDq�fDq��Dr� DsfDs�fDt  Dt� Du  Du�fDvfDv�fDw  Dwy�Dw�3Dy��D�&f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @���AffA"ffABffAbffA�33A�33A�33A�33A�33A�33A�33A�33B  B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B��B�L�B�L�B�L�B�L�B�L�B�L�BԀ B�L�B�L�B�� B� B�L�B�L�B�L�B�L�B�L�B�L�C &fC&fC&fC&fC&fC
&fC&fC&fC&fC&fC&fC&fC�C&fC&fC&fC &fC"&fC$&fC&&fC(�C*�C,&fC.@ C0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR�CT�CV&fCX&fCZ&fC\&fC^&fC`@ Cb&fCd&fCf�Ch�Cj&fCl&fCn&fCp@ Cr&fCt&fCv@ Cx&fCz&fC|&fC~&fC�3C�3C�3C�3C�3C�  C�3C�fC�  C�  C�3C�fC�fC�3C�  C�  C�fC�3C�3C�fC�fC�3C�  C�fC�fC�3C�3C�3C�fC�fC�3C�3C�fC�3C�  C�fC�fC�fC�3C�  C�3C�3C�3C�3C�  C�  C�3C�fC�fC�3C�  C�  C�fC�3C�  C�fC�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�,�C�  C�fC�3C�  C�3C�fC�  C�3C�fC�3C�fC�3C�  C�  C�  C�  C�3C�3C�fC�3C�  C�  C�3C�fC�fC���C�fC�fC�fC�fC�3C�  C�fC���C�fC�fC�3C�3C�fC�fC�fC�3C�3C�  C�3C�3C�  C�3C�3C�,�C�fC�3C�3C�fC�3C�3C�3C�3C�3D 	�D � D D��D	�D��D4D�4D	�D��D D��D4D��D	�D�4D	�D��D		�D	�4D
 D
�4D D��D D��D4D��D4D� D4D� D4D��D4D� D4D��D	�D� D D��D	�D��D	�D� D4D��D	�D�4D	�D� D	�D��D D� D4D��D	�D)	�D)��D*	�D*��D+	�D+��D,	�D,��D-4D-�4D.4D.�4D/ D/�gD0 D0�4D1 D1��D24D2� D3	�D3�4D4 D4�4D5 D5��D6 D6��D7 D7��D8 D8��D9	�D9�4D:	�D:��D;	�D;� D< D<� D=	�D=�4D> D>�4D?	�D?� D@	�D@��DA	�DA|�DB	�DB� DC DC��DD DD��DE	�DE��DF DF��DG4DG��DH	�DH�4DI4DI��DJ DJ� DK	�DK� DL	�DL��DM	�DM��DN	�DN�4DO DO� DP	�DP��DQ	�DQ�4DR	�DR��DS	�DS��DT4DT�4DT��DU��DV	�DV��DW	�DW�4DX	�DX� DY	�DY��DZ	�DZ��D[	�D[� D\ D\��D]	�D]��D^4D^|�D_4D_� D`	�D`��Da	�Da��Db	�Db��Dc	�Dc��Dd	�Dd��De De� Df	�Df�4Dg	�Dg� Dh	�Dh��Di	�Di�4Dj	�Dj� Dk Dk� Dl Dl�gDm	�Dm�4Dn Dn��Do	�Do� Dp	�Dp��Dq Dq� Dr4Dr��Ds Ds� Dt	�Dt��Du	�Du� Dv Dv� Dw	�Dw�4Dw��Dy�]D�+3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�9XA�&�A�VA�
=A�1A�1A�%A�A�A�  A���A���A���A��A��mA��/A���A˸RA�n�A��A���A�"�A�r�A��#A�A���AƟ�A�K�A�A�A�1'A�oA�JA���A�ƨAŸRAť�A�x�A�x�A�v�A�`BA��A�ĜA��mA���AĲ-Aġ�Ać+A�z�A�p�A�hsA�S�A�9XA�&�A��A��A�1Aò-A�JA��A�ȴA�;dA���A��A���A�hsA���A��A���A���A�z�A���A�oA��\A��`A��;A��A��DA�\)A�l�A���A���A� �A�7LA�E�A�(�A�?}A���A��HA��A�
=A��A�v�A�
=A�1A�?}A�G�A�t�A���A���A��wA��mA���A�(�A��A���A�t�A��A��A��jA~bNA}�A|��Az^5Av�/At��AsG�ApĜAoAodZAo?}Am��AjjAd��Aa��A`r�A_XA[�PAY�-AU�TAS"�AQ/AO�AN��AKt�AJv�AH��AE�wAB��A?�
A>��A<z�A97LA8JA77LA6��A6^5A4��A2 �A0-A/"�A.�DA-�TA+��A(n�A%�PA$~�A$1A#S�A"��A!��At�A=qAG�A�^A�AK�A�!A9XAO�A��A�#A��AS�A  A�A��A$�A1AG�A
�RA
9XA
E�A
n�A
  A	%A�uA-A��AA=qA�^A�A��A��A;dAVA�AffA��A ��@�ƨ@��y@�1'@�
=@���@��@�~�@���@�@��#@��^@��@��@���@�x�@�9X@�A�@�\)@�K�@���@��D@�(�@�o@�=q@�@��@�z�@�b@�\)@�V@٩�@�&�@؛�@� �@�|�@�C�@ա�@� �@�K�@���@љ�@�O�@���@�b@�C�@θR@�^5@�J@��/@�9X@ʇ+@ȓu@�-@�Z@þw@�@�-@�@+@���@�`B@�t�@�/@�`B@��D@�t�@�O�@��D@���@���@�@��H@�ff@�z�@�1@�|�@��@���@��j@�b@��
@��w@�t�@�K�@�ȴ@�ff@�-@��7@�bN@�;d@�@���@���@���@��+@�M�@��T@�7L@���@�9X@�1@��@�S�@�"�@��@��y@���@�V@�{@���@��T@���@�7L@�@��@���@�@��@�O�@��9@��j@�z�@��@���@���@�+@�
=@�@��y@��R@��!@���@���@�v�@���@�?}@�hs@�X@��j@���@�bN@�9X@��w@�K�@��@�5?@��@��j@�z�@�1'@���@�S�@��@�~�@�@���@���@���@�V@���@���@�K�@��\@�{@���@�p�@�G�@��9@�9X@�A�@��m@��
@��w@���@�dZ@�;d@��@���@��@�ȴ@���@���@���@���@��!@��!@���@��+@�ff@�M�@�M�@�M�@�M�@�M�@�=q@�-@�@��^@���@�ƨ@�\)@�+@��@��@�v�@�M�@�-@�@��#@���@�hs@�&�@���@�j@�I�@�b@�  @�ƨ@�dZ@�K�@�K�@�"�@�
=@�+@��@�
=@�
=@��@���@��!@���@��!@���@��T@��@�j@���@�\)@�+@��y@���@��\@�M�@�@�@��7@�x�@�G�@��@��j@�Z@�9X@���@��w@��@���@�|�@�dZ@�K�@�;d@�"�@��H@�=q@���@���@��h@�x�@�X@�/@��@��@��@�O@l��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�9XA�&�A�VA�
=A�1A�1A�%A�A�A�  A���A���A���A��A��mA��/A���A˸RA�n�A��A���A�"�A�r�A��#A�A���AƟ�A�K�A�A�A�1'A�oA�JA���A�ƨAŸRAť�A�x�A�x�A�v�A�`BA��A�ĜA��mA���AĲ-Aġ�Ać+A�z�A�p�A�hsA�S�A�9XA�&�A��A��A�1Aò-A�JA��A�ȴA�;dA���A��A���A�hsA���A��A���A���A�z�A���A�oA��\A��`A��;A��A��DA�\)A�l�A���A���A� �A�7LA�E�A�(�A�?}A���A��HA��A�
=A��A�v�A�
=A�1A�?}A�G�A�t�A���A���A��wA��mA���A�(�A��A���A�t�A��A��A��jA~bNA}�A|��Az^5Av�/At��AsG�ApĜAoAodZAo?}Am��AjjAd��Aa��A`r�A_XA[�PAY�-AU�TAS"�AQ/AO�AN��AKt�AJv�AH��AE�wAB��A?�
A>��A<z�A97LA8JA77LA6��A6^5A4��A2 �A0-A/"�A.�DA-�TA+��A(n�A%�PA$~�A$1A#S�A"��A!��At�A=qAG�A�^A�AK�A�!A9XAO�A��A�#A��AS�A  A�A��A$�A1AG�A
�RA
9XA
E�A
n�A
  A	%A�uA-A��AA=qA�^A�A��A��A;dAVA�AffA��A ��@�ƨ@��y@�1'@�
=@���@��@�~�@���@�@��#@��^@��@��@���@�x�@�9X@�A�@�\)@�K�@���@��D@�(�@�o@�=q@�@��@�z�@�b@�\)@�V@٩�@�&�@؛�@� �@�|�@�C�@ա�@� �@�K�@���@љ�@�O�@���@�b@�C�@θR@�^5@�J@��/@�9X@ʇ+@ȓu@�-@�Z@þw@�@�-@�@+@���@�`B@�t�@�/@�`B@��D@�t�@�O�@��D@���@���@�@��H@�ff@�z�@�1@�|�@��@���@��j@�b@��
@��w@�t�@�K�@�ȴ@�ff@�-@��7@�bN@�;d@�@���@���@���@��+@�M�@��T@�7L@���@�9X@�1@��@�S�@�"�@��@��y@���@�V@�{@���@��T@���@�7L@�@��@���@�@��@�O�@��9@��j@�z�@��@���@���@�+@�
=@�@��y@��R@��!@���@���@�v�@���@�?}@�hs@�X@��j@���@�bN@�9X@��w@�K�@��@�5?@��@��j@�z�@�1'@���@�S�@��@�~�@�@���@���@���@�V@���@���@�K�@��\@�{@���@�p�@�G�@��9@�9X@�A�@��m@��
@��w@���@�dZ@�;d@��@���@��@�ȴ@���@���@���@���@��!@��!@���@��+@�ff@�M�@�M�@�M�@�M�@�M�@�=q@�-@�@��^@���@�ƨ@�\)@�+@��@��@�v�@�M�@�-@�@��#@���@�hs@�&�@���@�j@�I�@�b@�  @�ƨ@�dZ@�K�@�K�@�"�@�
=@�+@��@�
=@�
=@��@���@��!@���@��!@���@��T@��@�j@���@�\)@�+@��y@���@��\@�M�@�@�@��7@�x�@�G�@��@��j@�Z@�9X@���@��w@��@���@�|�@�dZ@�K�@�;d@�"�@��H@�=q@���@���@��h@�x�@�X@�/@��@��@��@�O@l��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�
B
�B
�B
�B
�#B
�B
�B
�#B
�)B
�/B
�5B
�;B
�BB
�BB
�NB
�B1B�B%�B<jBJ�BL�BR�BjBr�B�B�B�%B�B�B�B� B}�B|�B~�B�B�+B�+B�=B�bB��B�}B�)B�B�B��B��B%B
=BPBJB
=B	7B%B	7B'�B33B?}BN�BT�B[#BbNBk�Bq�By�B�7B�7B�1B�B�%B�+B�\B�7B�B�B�B~�B{�B� B~�Bz�By�Bv�Bn�BffBQ�BF�B.B�B��B��B��Bu�BQ�B%�B�BB
�B
�NB
�/B
�B
ĜB
��B
�oB
|�B
^5B
>wB
$�B
�B
oB
B	�B	�BB	�B	ɺB	B	��B	�}B	�?B	��B	� B	p�B	ffB	]/B	J�B	>wB	'�B	�B	JB	B��B�B�ZB�)B��BŢB�dB�?B�B��B��B��B��B��B��B��B��B��B��B��B�=B�1B�B�\B�uB�{B�hB�PB�%B�B{�Bv�Bm�BhsBffBe`BffBffBffBe`BdZBdZBcTBe`BhsBs�Bx�B|�B|�B�B�\B��B��B��B��B��B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�bB�oB��B��B��B��B�B�B�B�!B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�3B�RB�^B�dB�jB�qB�wBBĜBŢBǮBǮB��B��BɺBȴBɺB��B��B��B��B��B�#B�NB�TB�HB�HB�fB�mB�yB�`B�sB�B��B	B	JB	{B	�B	!�B	"�B	#�B	$�B	.B	2-B	49B	49B	6FB	6FB	:^B	=qB	>wB	B�B	I�B	Q�B	S�B	T�B	T�B	W
B	YB	ZB	^5B	aHB	bNB	e`B	hsB	hsB	hsB	hsB	hsB	jB	l�B	o�B	r�B	r�B	s�B	v�B	x�B	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�FB	�dB	�dB	�dB	�jB	�jB	�jB	�dB	�XB	�jB	��B	��B	��B	��B	ÖB	ÖB	ĜB	ĜB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�#B	�B	�#B	�)B	�5B	�BB	�HB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
+B
+B
1B
+B
1B
1B
	7B
1B
	7B
	7B

=B

=B
DB
DB
PB
VB
\B
\B
\B
\B
\B
bB
hB
hB
hB
hB
oB
oB
oB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
'R222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
�B
�B
�
B
�B
�B
�B
�#B
�B
�B
�#B
�)B
�/B
�5B
�;B
�BB
�BB
�NB
�B1B�B%�B<jBJ�BL�BR�BjBr�B�B�B�%B�B�B�B� B}�B|�B~�B�B�+B�+B�=B�bB��B�}B�)B�B�B��B��B%B
=BPBJB
=B	7B%B	7B'�B33B?}BN�BT�B[#BbNBk�Bq�By�B�7B�7B�1B�B�%B�+B�\B�7B�B�B�B~�B{�B� B~�Bz�By�Bv�Bn�BffBQ�BF�B.B�B��B��B��Bu�BQ�B%�B�BB
�B
�NB
�/B
�B
ĜB
��B
�oB
|�B
^5B
>wB
$�B
�B
oB
B	�B	�BB	�B	ɺB	B	��B	�}B	�?B	��B	� B	p�B	ffB	]/B	J�B	>wB	'�B	�B	JB	B��B�B�ZB�)B��BŢB�dB�?B�B��B��B��B��B��B��B��B��B��B��B��B�=B�1B�B�\B�uB�{B�hB�PB�%B�B{�Bv�Bm�BhsBffBe`BffBffBffBe`BdZBdZBcTBe`BhsBs�Bx�B|�B|�B�B�\B��B��B��B��B��B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�bB�oB��B��B��B��B�B�B�B�!B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�3B�RB�^B�dB�jB�qB�wBBĜBŢBǮBǮB��B��BɺBȴBɺB��B��B��B��B��B�#B�NB�TB�HB�HB�fB�mB�yB�`B�sB�B��B	B	JB	{B	�B	!�B	"�B	#�B	$�B	.B	2-B	49B	49B	6FB	6FB	:^B	=qB	>wB	B�B	I�B	Q�B	S�B	T�B	T�B	W
B	YB	ZB	^5B	aHB	bNB	e`B	hsB	hsB	hsB	hsB	hsB	jB	l�B	o�B	r�B	r�B	s�B	v�B	x�B	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�FB	�dB	�dB	�dB	�jB	�jB	�jB	�dB	�XB	�jB	��B	��B	��B	��B	ÖB	ÖB	ĜB	ĜB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�#B	�B	�#B	�)B	�5B	�BB	�HB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
+B
+B
1B
+B
1B
1B
	7B
1B
	7B
	7B

=B

=B
DB
DB
PB
VB
\B
\B
\B
\B
\B
bB
hB
hB
hB
hB
oB
oB
oB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
'R222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.15 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191650                              AO  ARCAADJP                                                                    20181005191650    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191650  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191650  QCF$                G�O�G�O�G�O�8000            