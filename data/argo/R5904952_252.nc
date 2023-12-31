CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:02Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190602  20181005190602  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��iҭ�!1   @��jl�f@1vE�����c��1&�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@���@���A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B��B��B��B   B(  B0  B8ffB@��BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B���B���B�  B�  B�  C   C  C  C  C  C
  C  C�C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� DfD� D  D� DfD�fD	  D	� D
  D
� D  D� D  Dy�D��D� D  D� D  D� DfD� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D��D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$fD$�fD%  D%� D&  D&� D'fD'� D(  D(�fD)  D)� D*  D*� D+  D+� D,  D,y�D-  D-� D-��D.� D/fD/�fD0fD0� D1  D1� D2fD2� D2��D3y�D3��D4y�D5  D5� D6  D6� D7fD7�fD8fD8� D9  D9�fD:  D:� D;fD;�fD<  D<� D=  D=� D>  D>� D?  D?� D@fD@� D@��DA� DB  DB� DC  DC� DC��DDy�DE  DE� DF  DFy�DG  DG� DG��DHy�DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DO��DP� DQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU� DV  DVy�DW  DW� DX  DXy�DY  DY�fDZfDZ� D[  D[� D[��D\� D]  D]� D^  D^�fD_fD_�fD`fD`� Da  Da� Db  Db� Dc  Dc�fDd  Dd� De  De� Df  Df� Dg  Dg� DhfDh�fDh��Di� DjfDj� Dj��Dky�Dl  Dl�fDmfDm�fDnfDn�fDo  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwl�Dy\�D�1�D�~11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@���A ��A"ffABffAbffA�33A�33A�  A�33A�33A�33A�33A�33B ��B34B34B34B ��B(��B0��B9  BAfgBH��BP��BX��B`��Bh��Bp34Bx��B�L�B�L�B�L�B�L�B�L�B��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B��B�L�B�L�B�L�B�L�B��B��B�L�B�L�B�L�C &fC&fC&fC&fC&fC
&fC&fC@ C@ C&fC&fC&fC&fC&fC&fC&fC &fC"&fC$&fC&&fC(&fC*&fC,@ C.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd&fCf&fCh&fCj@ Cl&fCn&fCp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�  C�3C�3C�3C�3C�3C�fC�fC�3C�3C�3C�fC�3C�3C�3C�3C�3C�3C�fC�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�fC�fC�3C�3C�fC�fC�3C�3C�3C�3C�  C�3C�3C�  C�  C�  C�3C�3C�3C�3C�3C�3C�fC�3C�  C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D ��D	�D��D	�D��D	�D��D	�D��D	�D��D D��D	�D��D D� D		�D	��D
	�D
��D	�D��D	�D�4D4D��D	�D��D	�D��D D��D	�D��D	�D��D	�D��D4D��D	�D��D	�D��D	�D��D4D��D	�D��D	�D��D D� D	�D��D	�D��D	�D��D	�D��D 	�D ��D!	�D!��D"	�D"��D#	�D#��D$ D$� D%	�D%��D&	�D&��D' D'��D(	�D(� D)	�D)��D*	�D*��D+	�D+��D,	�D,�4D-	�D-��D.4D.��D/ D/� D0 D0��D1	�D1��D2 D2��D34D3�4D44D4�4D5	�D5��D6	�D6��D7 D7� D8 D8��D9	�D9� D:	�D:��D; D;� D<	�D<��D=	�D=��D>	�D>��D?	�D?��D@ D@��DA4DA��DB	�DB��DC	�DC��DD4DD�4DE	�DE��DF	�DF�4DG	�DG��DH4DH�4DI	�DI� DJ	�DJ��DK	�DK��DL	�DL��DM	�DM��DN	�DN��DO	�DO��DP4DP��DQ	�DQ��DR DR��DS	�DS��DT	�DT��DU	�DU��DV	�DV�4DW	�DW��DX	�DX�4DY	�DY� DZ DZ��D[	�D[��D\4D\��D]	�D]��D^	�D^� D_ D_� D` D`��Da	�Da��Db	�Db��Dc	�Dc� Dd	�Dd��De	�De��Df	�Df��Dg	�Dg��Dh Dh� Di4Di��Dj Dj��Dk4Dk�4Dl	�Dl� Dm Dm� Dn Dn� Do	�Do��Dp	�Dp��Dq	�Dq�4Dr	�Dr��Ds	�Ds��Dt	�Dt��Du	�Du��Dv	�Dv��Dw	�DwvgDyfgD�6gD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�JA�oA�bA��A�$�A�$�A��A��A� �A�$�A�$�A�$�A� �A��A�bA��`A�^5AǮA�dZA�C�A�=qA�5?A�7LA� �A�JA�A���A��`A���AƶFA�p�A�S�A��AžwA�ffA�bNA�^5A�l�A��yA��A��jA�n�A���A��/A�E�A�|�A�A�x�A��A��HA�C�A��!A�~�A�hsA��A��PA���A�;dA���A��-A��A��RA�$�A�jA�A�A��!A�jA�1A�ffA��FA�?}A�`BA��jA��yA��RA�1A��A�$�A��A�I�A���A�S�A�-A���A�p�A��RA���A�+A�(�A���A�XA��\A�ffA��mA�K�A�{A�ƨA�%A���A���A~�!A{XAz��Az(�AxȴAu��Aq��Al�DAf�Aa��A^bA\M�AZ~�AW�^AU7LAS��AR��AP��AN�AM�FAMK�AL��AK�AIl�AG�ACG�A>�A<1'A933A7G�A41A333A1��A0ȴA0�A0v�A/��A/�A.�!A.Q�A-33A+��A*�+A*A�A*�A)��A)��A)x�A)t�A)hsA'�;A&ffA#p�A!G�A�wA�/AbA�A1'A�FA��AhsA��At�A��AƨA�^A�A;dA�TA��A�A�A��AG�AbA|�A(�A
�uA	��A�9A�jA	�A��A��Al�AĜA��A��A�`A�A~�A33A�DAI�A-A  Al�A�A7LA ~�@�V@�V@�"�@���@�\)@��m@�~�@�Ĝ@��@�dZ@��y@���@���@�O�@�&�@�Z@��@�ƨ@�\)@�C�@�dZ@��m@� �@��/@�M�@�$�@���@�Z@�dZ@��@���@�~�@���@�@�j@@�%@�j@�9@�!@�h@��@�ff@��@�ff@�!@�V@�?}@�j@�I�@�@��@��@�(�@�Z@��@�?}@��@��@ݑh@ܴ9@�Q�@��@�dZ@��@؃@���@��@���@�V@�$�@��@�Ĝ@�Z@���@ӍP@�l�@�@�n�@ѡ�@мj@Л�@��@�/@�V@�Z@�z�@��@мj@�o@�ff@�^5@�V@���@�?}@̬@� �@̴9@��@���@�1'@�o@ʏ\@��#@ɲ-@ə�@��/@�ƨ@�v�@�/@ēu@�1'@��
@î@�l�@��@�{@�V@���@���@��`@��@��@�33@�n�@���@���@���@�`B@�%@���@�Q�@��m@���@�C�@���@�ff@��#@��7@�G�@�I�@��@�1@��
@�dZ@�^5@���@��@��`@�bN@�A�@�(�@���@�t�@���@���@�ff@��@��@���@�x�@���@�z�@�Q�@�A�@�(�@��@��@��m@�;d@��R@���@�v�@�-@��@��^@���@�`B@�V@��@��j@�1@��@�t�@���@���@��7@�?}@��/@���@��@�Z@�\)@�K�@�33@�~�@�$�@�@���@��-@���@��7@�O�@��`@�j@�  @��@�dZ@�K�@�+@���@���@�n�@�@��-@��^@���@��@�p�@�G�@��9@��u@��@�1'@��;@�S�@�;d@�+@��@�@�ff@�{@�J@���@���@�`B@�G�@�7L@���@�Q�@���@���@�l�@�C�@�33@�+@�
=@���@��@��!@�E�@�J@���@��#@���@���@��-@��@�`B@�/@�7L@�G�@�G�@��@���@�r�@� �@��m@��@�\)@��@�
=@��@��R@�ff@�{@��@���@�hs@�G�@��@���@��/@��@��u@��@�r�@�Z@�A�@��m@�dZ@�S�@�;d@�Ta@���@q�d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�JA�oA�bA��A�$�A�$�A��A��A� �A�$�A�$�A�$�A� �A��A�bA��`A�^5AǮA�dZA�C�A�=qA�5?A�7LA� �A�JA�A���A��`A���AƶFA�p�A�S�A��AžwA�ffA�bNA�^5A�l�A��yA��A��jA�n�A���A��/A�E�A�|�A�A�x�A��A��HA�C�A��!A�~�A�hsA��A��PA���A�;dA���A��-A��A��RA�$�A�jA�A�A��!A�jA�1A�ffA��FA�?}A�`BA��jA��yA��RA�1A��A�$�A��A�I�A���A�S�A�-A���A�p�A��RA���A�+A�(�A���A�XA��\A�ffA��mA�K�A�{A�ƨA�%A���A���A~�!A{XAz��Az(�AxȴAu��Aq��Al�DAf�Aa��A^bA\M�AZ~�AW�^AU7LAS��AR��AP��AN�AM�FAMK�AL��AK�AIl�AG�ACG�A>�A<1'A933A7G�A41A333A1��A0ȴA0�A0v�A/��A/�A.�!A.Q�A-33A+��A*�+A*A�A*�A)��A)��A)x�A)t�A)hsA'�;A&ffA#p�A!G�A�wA�/AbA�A1'A�FA��AhsA��At�A��AƨA�^A�A;dA�TA��A�A�A��AG�AbA|�A(�A
�uA	��A�9A�jA	�A��A��Al�AĜA��A��A�`A�A~�A33A�DAI�A-A  Al�A�A7LA ~�@�V@�V@�"�@���@�\)@��m@�~�@�Ĝ@��@�dZ@��y@���@���@�O�@�&�@�Z@��@�ƨ@�\)@�C�@�dZ@��m@� �@��/@�M�@�$�@���@�Z@�dZ@��@���@�~�@���@�@�j@@�%@�j@�9@�!@�h@��@�ff@��@�ff@�!@�V@�?}@�j@�I�@�@��@��@�(�@�Z@��@�?}@��@��@ݑh@ܴ9@�Q�@��@�dZ@��@؃@���@��@���@�V@�$�@��@�Ĝ@�Z@���@ӍP@�l�@�@�n�@ѡ�@мj@Л�@��@�/@�V@�Z@�z�@��@мj@�o@�ff@�^5@�V@���@�?}@̬@� �@̴9@��@���@�1'@�o@ʏ\@��#@ɲ-@ə�@��/@�ƨ@�v�@�/@ēu@�1'@��
@î@�l�@��@�{@�V@���@���@��`@��@��@�33@�n�@���@���@���@�`B@�%@���@�Q�@��m@���@�C�@���@�ff@��#@��7@�G�@�I�@��@�1@��
@�dZ@�^5@���@��@��`@�bN@�A�@�(�@���@�t�@���@���@�ff@��@��@���@�x�@���@�z�@�Q�@�A�@�(�@��@��@��m@�;d@��R@���@�v�@�-@��@��^@���@�`B@�V@��@��j@�1@��@�t�@���@���@��7@�?}@��/@���@��@�Z@�\)@�K�@�33@�~�@�$�@�@���@��-@���@��7@�O�@��`@�j@�  @��@�dZ@�K�@�+@���@���@�n�@�@��-@��^@���@��@�p�@�G�@��9@��u@��@�1'@��;@�S�@�;d@�+@��@�@�ff@�{@�J@���@���@�`B@�G�@�7L@���@�Q�@���@���@�l�@�C�@�33@�+@�
=@���@��@��!@�E�@�J@���@��#@���@���@��-@��@�`B@�/@�7L@�G�@�G�@��@���@�r�@� �@��m@��@�\)@��@�
=@��@��R@�ff@�{@��@���@�hs@�G�@��@���@��/@��@��u@��@�r�@�Z@�A�@��m@�dZ@�S�@�;d@�Ta@���@q�d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�jB�jB�jB�dB�dB�dB�dB�^B�dB�dB�dB�dB�^B�XB�RB�3B��B��B�RBŢBɺB��B�#B��B	PB	!�B	,B	dZB	�}B
B
]/B
o�B
�B
ɺB
�B+B(�BH�B`BBdZBhsBr�B�B�B�\B��B�?B�9B�qB��BBȴB�5B�B�B�B�B�sB�NB��B�!BÖBB0!B8RBE�BQ�BVBJ�BM�BM�BG�BJ�B\)BXBE�B%�B+B��B�9B��B�PBx�BgmBS�B?}B#�B{B
��B
�B
��B
dZB
r�B
z�B
W
B
E�B
?}B
1'B
bB	��B	�B	ĜB	�qB	�XB	�B	��B	� B	_;B	=qB	%�B	VB	B��B�B�TB�5B�5B�ZB��B	B	\B	�B	"�B	"�B		7B�B�BB�FB�9B�FB�9B�?B�?B�?B�9B�?B�?B�?B�9B�?B�RB�jB��BÖBȴB��B��B��B��B�B�B�#B��BŢB��B�jB�LB�FB�9B�LB�dB�^B�^B�wBBBĜBŢB��B��B�
B�B�B�B�
B�B�
B�B�5B�`B��B	
=B	\B	DB	+B	%B	JB	hB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	{B	oB	hB	{B	hB	VB	\B	!�B	:^B	;dB	9XB	B�B	D�B	F�B	J�B	M�B	O�B	T�B	T�B	VB	XB	YB	]/B	gmB	o�B	y�B	�DB	�DB	�DB	�PB	�\B	��B	��B	��B	��B	��B	�hB	�DB	� B	o�B	`BB	ZB	XB	W
B	aHB	|�B	�B	�7B	�=B	�JB	�VB	�oB	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�?B	�?B	�?B	�LB	�RB	�XB	�^B	�RB	�LB	�FB	�9B	�3B	�3B	�3B	�FB	�dB	�}B	��B	�}B	B	ǮB	ƨB	ŢB	ĜB	ŢB	ŢB	ÖB	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	��B	�
B	�
B	�
B	�B	�B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�BB	�HB	�TB	�ZB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
DB
DB
DB
DB
DB
DB
DB
JB
PB
PB
VB
\B
\B
bB
hB
hB
hB
hB
bB
hB
hB
bB
hB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
2�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B�jB�jB�jB�dB�dB�dB�dB�^B�dB�dB�dB�dB�^B�XB�RB�3B��B��B�RBŢBɺB��B�#B��B	PB	!�B	,B	dZB	�}B
B
]/B
o�B
�B
ɺB
�B+B(�BH�B`BBdZBhsBr�B�B�B�\B��B�?B�9B�qB��BBȴB�5B�B�B�B�B�sB�NB��B�!BÖBB0!B8RBE�BQ�BVBJ�BM�BM�BG�BJ�B\)BXBE�B%�B+B��B�9B��B�PBx�BgmBS�B?}B#�B{B
��B
�B
��B
dZB
r�B
z�B
W
B
E�B
?}B
1'B
bB	��B	�B	ĜB	�qB	�XB	�B	��B	� B	_;B	=qB	%�B	VB	B��B�B�TB�5B�5B�ZB��B	B	\B	�B	"�B	"�B		7B�B�BB�FB�9B�FB�9B�?B�?B�?B�9B�?B�?B�?B�9B�?B�RB�jB��BÖBȴB��B��B��B��B�B�B�#B��BŢB��B�jB�LB�FB�9B�LB�dB�^B�^B�wBBBĜBŢB��B��B�
B�B�B�B�
B�B�
B�B�5B�`B��B	
=B	\B	DB	+B	%B	JB	hB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	{B	oB	hB	{B	hB	VB	\B	!�B	:^B	;dB	9XB	B�B	D�B	F�B	J�B	M�B	O�B	T�B	T�B	VB	XB	YB	]/B	gmB	o�B	y�B	�DB	�DB	�DB	�PB	�\B	��B	��B	��B	��B	��B	�hB	�DB	� B	o�B	`BB	ZB	XB	W
B	aHB	|�B	�B	�7B	�=B	�JB	�VB	�oB	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�?B	�?B	�?B	�LB	�RB	�XB	�^B	�RB	�LB	�FB	�9B	�3B	�3B	�3B	�FB	�dB	�}B	��B	�}B	B	ǮB	ƨB	ŢB	ĜB	ŢB	ŢB	ÖB	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	��B	�
B	�
B	�
B	�B	�B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�BB	�HB	�TB	�ZB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
DB
DB
DB
DB
DB
DB
DB
JB
PB
PB
VB
\B
\B
bB
hB
hB
hB
hB
bB
hB
hB
bB
hB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
2�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.15 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190602                              AO  ARCAADJP                                                                    20181005190602    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190602  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190602  QCF$                G�O�G�O�G�O�8000            