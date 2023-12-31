CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:01Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190601  20181005190601  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�����D1   @���K�'�@1J=p��
�c�O�;dZ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C=�fC@  CB�CD�CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Ck�fCn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C�  C�  C�  C��3C��3C��3C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C��3C�  C�  D   D � DfD� DfD� D  D� D  Dy�D��D� D  D� DfD�fD  D� D	  D	� D	��D
� D  D� D  D� D  D� D  D� D  D�fD  Dy�D  D�fD  D� D  Dy�D  D� D  D�fD��D� D  D� D  D�fD  D� D  D� D  Dy�D  D�fD  D� D  D� D  D�fD   D � D ��D!� D"fD"�fD#  D#y�D$  D$�fD%  D%� D&  D&� D'  D'y�D(  D(� D)  D)� D*  D*� D*��D+� D,  D,� D-  D-y�D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D3��D4� D5  D5� D6  D6� D7  D7� D8  D8y�D8��D9� D:  D:y�D:��D;y�D;��D<y�D=  D=� D>fD>�fD?  D?� D@  D@� D@��DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DIy�DJ  DJ� DK  DK�fDLfDL�fDM  DM� DN  DN� DO  DO�fDP  DP� DQ  DQy�DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DYfDY� DZ  DZ� D[  D[�fD\fD\�fD]fD]� D^  D^� D_  D_� D`  D`� Da  Da� Da��Dby�Db��Dc� Dd  Ddy�Dd��De� Df  Df�fDgfDgy�Dh  Dh� Dh��Di� Dj  Dj� Dk  Dk� Dl  Dl�fDmfDm� Dn  Dn� DofDo�fDp  Dpy�Dq  Dq� DrfDr� Ds  Ds� DtfDt�fDu  Du� Dv  Dv� Dw  Dwy�Dyl�D�3�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @<��@���@���AffA"ffABffAbffA�33A�33A�33A�ffA�33A�33A�33A�33B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B�L�B�L�B�L�B��B�L�B�L�B�L�B�L�B�L�B��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B܀ B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C &fC&fC&fC&fC&fC
&fC&fC&fC&fC@ C@ C&fC&fC&fC&fC&fC &fC"&fC$&fC&&fC(&fC*&fC,&fC.�C0&fC2&fC4&fC6&fC8&fC:&fC<&fC>�C@&fCB@ CD@ CF@ CH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd&fCf&fCh&fCj&fCl�Cn&fCp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�  C�  C�3C�3C�fC�3C�3C�3C�3C�fC�fC�fC�3C�3C�  C�3C�fC�3C�3C�3C�3C�3C�  C�3C�fC�3C�3C�fC�3C�3C�3C�3C�3C�3C�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�fC�fC�3C�3C�3C�fC�3C�3D 	�D ��D D��D D��D	�D��D	�D�4D4D��D	�D��D D� D	�D��D		�D	��D
4D
��D	�D��D	�D��D	�D��D	�D��D	�D� D	�D�4D	�D� D	�D��D	�D�4D	�D��D	�D� D4D��D	�D��D	�D� D	�D��D	�D��D	�D�4D	�D� D	�D��D	�D��D	�D� D 	�D ��D!4D!��D" D"� D#	�D#�4D$	�D$� D%	�D%��D&	�D&��D'	�D'�4D(	�D(��D)	�D)��D*	�D*��D+4D+��D,	�D,��D-	�D-�4D.	�D.��D/	�D/��D0	�D0��D1	�D1� D2	�D2��D3	�D3��D44D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8�4D94D9��D:	�D:�4D;4D;�4D<4D<�4D=	�D=��D> D>� D?	�D?��D@	�D@��DA4DA��DB	�DB��DC	�DC��DD	�DD��DE	�DE��DF	�DF��DG	�DG��DH	�DH��DI	�DI�4DJ	�DJ��DK	�DK� DL DL� DM	�DM��DN	�DN��DO	�DO� DP	�DP��DQ	�DQ�4DR4DR��DS	�DS��DT	�DT��DU	�DU��DV	�DV��DW	�DW��DX	�DX��DY DY��DZ	�DZ��D[	�D[� D\ D\� D] D]��D^	�D^��D_	�D_��D`	�D`��Da	�Da��Db4Db�4Dc4Dc��Dd	�Dd�4De4De��Df	�Df� Dg Dg�4Dh	�Dh��Di4Di��Dj	�Dj��Dk	�Dk��Dl	�Dl� Dm Dm��Dn	�Dn��Do Do� Dp	�Dp�4Dq	�Dq��Dr Dr��Ds	�Ds��Dt Dt� Du	�Du��Dv	�Dv��Dw	�Dw�4DyvgD�8�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A�ƨA�A�ĜA�A�ƨAʺ^A�l�AȋDA�|�A�VA�K�A�M�A�K�A�C�A�+A�$�A�Q�A�v�A�|�A�x�A�v�A�9XA���A�
=Aģ�A�33A�oA��A�n�A�  A�A�z�A�JA��TA��A���A��FA�jA�  A���A��A�\)A�M�A�5?A��A��A��A���A���A��PA�r�A�/A��A���A��A�ƨA���A�ffA�|�A���A���A�z�A�n�A�{A�bNA��A���A���A��HA�A�9XA��#A��-A�1'A��mA�K�A��
A��A�1A�=qA�x�A��9A�A�5?A��RA��+A�A�A�{A��A���A��A�E�A��^A���A�p�A��7A�r�A�ĜA�+A�n�A�|�A�x�A�VA}��A|Q�Ay��Au?}ArQ�Ao�AljAehsA_�A\VA[/AW`BAUO�ATz�AS�#AS\)ASARbNAP�+AO&�AM�AK&�AE�;AC�^A@�DA@{A>�9A;"�A7�A5�wA3�
A0{A.jA-�A+�A*ĜA)��A(��A&�yA"��A!�A!7LA 1'A��A7LA�AO�A�hA�AoA5?At�A�Av�AO�AbA-A-A��AO�A�A�A�A�A�TA�TAffAA�PA�A
��A
~�A
1'AZA��AS�A��A�AJA��A	��A	A	�wA	�A	7LA�HAv�AI�A�AG�A�A��AXA@���@�@�Ĝ@�1@���@��@��
@�S�@�|�@�Z@�{@�K�@�l�@�+@�@��+@��D@��y@�5?@��T@���@���@�=q@��j@�S�@��@�ƨ@��@��`@��@�x�@�p�@�A�@�?}@�"�@�v�@�5?@�@�@���@�9X@�t�@�33@�@�j@�  @�;d@�{@ݡ�@�x�@��@���@ܴ9@ܣ�@�bN@�Q�@ۅ@��#@��@���@���@��@�V@�%@��`@ش9@ؼj@؃@�Q�@�=q@�7L@���@��@Լj@ԛ�@�r�@�  @Ӆ@���@�V@д9@�9X@Ͼw@υ@��@�+@�1@��@�dZ@�
=@ΰ!@�M�@�V@Χ�@��@�x�@�V@��/@���@�Q�@�Q�@�1@��;@�|�@�"�@��H@ʟ�@�=q@Ɂ@�Ĝ@Ǯ@�+@�ȴ@��@ũ�@�\)@�@��@���@��u@�A�@�9X@�  @��m@���@���@�"�@���@�=q@���@��@�`B@�O�@�9X@�C�@�J@�X@�z�@� �@��@�+@��R@�M�@�-@�=q@��!@��@��\@���@�G�@��/@��@���@�|�@�"�@�M�@���@���@�?}@���@�Z@��@�\)@��y@�ff@��@�$�@�5?@��^@�&�@�%@���@���@�z�@��@�ƨ@��@�l�@�o@��+@�J@�G�@��`@�(�@��@���@�C�@��R@�^5@�5?@��@�G�@�%@���@�j@���@�t�@�S�@�+@���@��@��@��#@�G�@��j@�9X@���@��P@�K�@�@���@��+@�-@��@��`@���@��@�j@��@��@��P@�S�@��@���@�~�@�$�@���@���@�p�@�V@��/@��D@�A�@�9X@�b@��@��@�t�@�S�@�33@��@���@�@�&�@���@���@��u@��D@�r�@�(�@��@���@��@���@��R@�^5@�^5@�J@���@�Z@��F@�ƨ@��
@��m@��@���@�|�@�K�@��@��@��^@���@�&�@��j@���@��@��D@��@��m@��@�|�@��@�\)@�
=@�n�@�5?@�-@�{@�J@��@��@��T@�G�@�7L@��`@�j@��^@|�_11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A�ƨA�A�ĜA�A�ƨAʺ^A�l�AȋDA�|�A�VA�K�A�M�A�K�A�C�A�+A�$�A�Q�A�v�A�|�A�x�A�v�A�9XA���A�
=Aģ�A�33A�oA��A�n�A�  A�A�z�A�JA��TA��A���A��FA�jA�  A���A��A�\)A�M�A�5?A��A��A��A���A���A��PA�r�A�/A��A���A��A�ƨA���A�ffA�|�A���A���A�z�A�n�A�{A�bNA��A���A���A��HA�A�9XA��#A��-A�1'A��mA�K�A��
A��A�1A�=qA�x�A��9A�A�5?A��RA��+A�A�A�{A��A���A��A�E�A��^A���A�p�A��7A�r�A�ĜA�+A�n�A�|�A�x�A�VA}��A|Q�Ay��Au?}ArQ�Ao�AljAehsA_�A\VA[/AW`BAUO�ATz�AS�#AS\)ASARbNAP�+AO&�AM�AK&�AE�;AC�^A@�DA@{A>�9A;"�A7�A5�wA3�
A0{A.jA-�A+�A*ĜA)��A(��A&�yA"��A!�A!7LA 1'A��A7LA�AO�A�hA�AoA5?At�A�Av�AO�AbA-A-A��AO�A�A�A�A�A�TA�TAffAA�PA�A
��A
~�A
1'AZA��AS�A��A�AJA��A	��A	A	�wA	�A	7LA�HAv�AI�A�AG�A�A��AXA@���@�@�Ĝ@�1@���@��@��
@�S�@�|�@�Z@�{@�K�@�l�@�+@�@��+@��D@��y@�5?@��T@���@���@�=q@��j@�S�@��@�ƨ@��@��`@��@�x�@�p�@�A�@�?}@�"�@�v�@�5?@�@�@���@�9X@�t�@�33@�@�j@�  @�;d@�{@ݡ�@�x�@��@���@ܴ9@ܣ�@�bN@�Q�@ۅ@��#@��@���@���@��@�V@�%@��`@ش9@ؼj@؃@�Q�@�=q@�7L@���@��@Լj@ԛ�@�r�@�  @Ӆ@���@�V@д9@�9X@Ͼw@υ@��@�+@�1@��@�dZ@�
=@ΰ!@�M�@�V@Χ�@��@�x�@�V@��/@���@�Q�@�Q�@�1@��;@�|�@�"�@��H@ʟ�@�=q@Ɂ@�Ĝ@Ǯ@�+@�ȴ@��@ũ�@�\)@�@��@���@��u@�A�@�9X@�  @��m@���@���@�"�@���@�=q@���@��@�`B@�O�@�9X@�C�@�J@�X@�z�@� �@��@�+@��R@�M�@�-@�=q@��!@��@��\@���@�G�@��/@��@���@�|�@�"�@�M�@���@���@�?}@���@�Z@��@�\)@��y@�ff@��@�$�@�5?@��^@�&�@�%@���@���@�z�@��@�ƨ@��@�l�@�o@��+@�J@�G�@��`@�(�@��@���@�C�@��R@�^5@�5?@��@�G�@�%@���@�j@���@�t�@�S�@�+@���@��@��@��#@�G�@��j@�9X@���@��P@�K�@�@���@��+@�-@��@��`@���@��@�j@��@��@��P@�S�@��@���@�~�@�$�@���@���@�p�@�V@��/@��D@�A�@�9X@�b@��@��@�t�@�S�@�33@��@���@�@�&�@���@���@��u@��D@�r�@�(�@��@���@��@���@��R@�^5@�^5@�J@���@�Z@��F@�ƨ@��
@��m@��@���@�|�@�K�@��@��@��^@���@�&�@��j@���@��@��D@��@��m@��@�|�@��@�\)@�
=@�n�@�5?@�-@�{@�J@��@��@��T@�G�@�7L@��`@�j@��^@|�_11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	&�B	%�B	$�B	#�B	#�B	#�B	#�B	 �B��B�B�;B��B	1B	B		7B	oB	"�B	A�B	bNB	�B	�hB	��B	ŢB	��B	��B	��B
uB
E�B
dZB
x�B
�{B
�RB
��B
��B
�B
��B  BBhB�B\B�B�B�B�B)�B2-B;dB@�BA�B@�BA�BB�BG�BN�BO�BL�BJ�BH�BL�BZBu�B��B��B��B�3B��B�B��B��B��B��B�/B�B��B�B�B�B�mB�;B�B5?BE�B8RB#�B�BPB�;B��B��Bo�BXB8RB
��B
��B
��B
�B
}�B
� B
q�B
<jB
%�B
�B	�yB	��B	�'B	��B	��B	�B	k�B	Q�B	0!B��B��BĜB�dB�wBŢB��B��B��B��BƨB�B�B��B�B��BƨB��B�)B�
B��BƨB��B�dB�-B�!B�'B�FB�
B�)B�)B�B��BȴBƨBɺBȴBȴBĜB��B��B��BÖBÖB��B��BBƨB�;B�ZB�`B�yB�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	{B	#�B	9XB	L�B	[#B	`BB	bNB	e`B	gmB	iyB	e`B	gmB	aHB	Q�B	dZB	gmB	T�B	K�B	<jB	8RB	5?B	49B	7LB	:^B	@�B	H�B	O�B	_;B	o�B	{�B	� B	�B	�B	}�B	x�B	x�B	|�B	~�B	|�B	}�B	z�B	u�B	t�B	n�B	p�B	u�B	x�B	{�B	�=B	�PB	�\B	�1B	�+B	�=B	�PB	�PB	�VB	�\B	�VB	�VB	�JB	�DB	�7B	�1B	�%B	�B	�%B	�%B	�7B	�JB	�PB	�PB	�\B	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�!B	�-B	�^B	�qB	�jB	�dB	�jB	�jB	�}B	B	ŢB	ŢB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ƨB	ƨB	ȴB	ǮB	ƨB	��B	�qB	�XB	�dB	�jB	��B	ĜB	ĜB	ĜB	ĜB	ÖB	ĜB	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	ÖB	ŢB	ŢB	ÖB	��B	�wB	�qB	B	B	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�/B	�5B	�HB	�NB	�NB	�NB	�TB	�TB	�NB	�ZB	�`B	�`B	�mB	�sB	�sB	�yB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
	7B
	7B

=B
DB
PB
VB
\B
bB
hB
oB
hB
hB
hB
oB
hB
hB
\B
PB
JB
JB
JB
PB
PB
PB
VB
\B
\B
bB
bB
hB
uB
oB
hB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
,W22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B	&�B	%�B	$�B	#�B	#�B	#�B	#�B	 �B��B�B�;B��B	1B	B		7B	oB	"�B	A�B	bNB	�B	�hB	��B	ŢB	��B	��B	��B
uB
E�B
dZB
x�B
�{B
�RB
��B
��B
�B
��B  BBhB�B\B�B�B�B�B)�B2-B;dB@�BA�B@�BA�BB�BG�BN�BO�BL�BJ�BH�BL�BZBu�B��B��B��B�3B��B�B��B��B��B��B�/B�B��B�B�B�B�mB�;B�B5?BE�B8RB#�B�BPB�;B��B��Bo�BXB8RB
��B
��B
��B
�B
}�B
� B
q�B
<jB
%�B
�B	�yB	��B	�'B	��B	��B	�B	k�B	Q�B	0!B��B��BĜB�dB�wBŢB��B��B��B��BƨB�B�B��B�B��BƨB��B�)B�
B��BƨB��B�dB�-B�!B�'B�FB�
B�)B�)B�B��BȴBƨBɺBȴBȴBĜB��B��B��BÖBÖB��B��BBƨB�;B�ZB�`B�yB�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	{B	#�B	9XB	L�B	[#B	`BB	bNB	e`B	gmB	iyB	e`B	gmB	aHB	Q�B	dZB	gmB	T�B	K�B	<jB	8RB	5?B	49B	7LB	:^B	@�B	H�B	O�B	_;B	o�B	{�B	� B	�B	�B	}�B	x�B	x�B	|�B	~�B	|�B	}�B	z�B	u�B	t�B	n�B	p�B	u�B	x�B	{�B	�=B	�PB	�\B	�1B	�+B	�=B	�PB	�PB	�VB	�\B	�VB	�VB	�JB	�DB	�7B	�1B	�%B	�B	�%B	�%B	�7B	�JB	�PB	�PB	�\B	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�!B	�-B	�^B	�qB	�jB	�dB	�jB	�jB	�}B	B	ŢB	ŢB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ƨB	ƨB	ȴB	ǮB	ƨB	��B	�qB	�XB	�dB	�jB	��B	ĜB	ĜB	ĜB	ĜB	ÖB	ĜB	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	ÖB	ŢB	ŢB	ÖB	��B	�wB	�qB	B	B	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�/B	�5B	�HB	�NB	�NB	�NB	�TB	�TB	�NB	�ZB	�`B	�`B	�mB	�sB	�sB	�yB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
	7B
	7B

=B
DB
PB
VB
\B
bB
hB
oB
hB
hB
hB
oB
hB
hB
\B
PB
JB
JB
JB
PB
PB
PB
VB
\B
\B
bB
bB
hB
uB
oB
hB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
,W22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.15 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190601                              AO  ARCAADJP                                                                    20181005190601    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190601  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190601  QCF$                G�O�G�O�G�O�8000            