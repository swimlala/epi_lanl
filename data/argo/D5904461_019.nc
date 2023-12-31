CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:39Z AOML 3.0 creation; 2016-08-07T21:36:30Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221339  20160807143630  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_019                   2C  D   APEX                            6531                            072314                          846 @�*�5�1   @�*`@1{dZ��d(�\1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DFfDF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dyy�D�fD�L�D���D�� D�  D�Y�D��3D���D�3D�&fD���D�� D�	�D�@ D�s3D� D�3D�P D�3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@���AffA$  ABffAbffA�33A�33A�33A�33A�33A�33A�33A�ffB ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B�L�B�L�B�L�B�� B�L�B�L�B��B�L�B��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�� B�L�B�L�B�L�C &fC&fC&fC&fC&fC
&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC &fC"&fC$&fC&&fC(&fC*&fC,&fC.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN@ CP@ CR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd&fCf&fCh&fCj&fCl&fCn&fCp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D ��D	�D� D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D		�D	��D
	�D
��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D 	�D ��D!	�D!��D"	�D"��D#	�D#��D$	�D$��D%	�D%��D&	�D&��D'	�D'��D(	�D(��D)	�D)��D*	�D*��D+	�D+��D,	�D,��D-	�D-��D.	�D.��D/	�D/��D0	�D0��D1	�D1��D2	�D2��D3	�D3��D4	�D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8��D9	�D9��D:	�D:��D;	�D;��D<	�D<��D=	�D=��D>	�D>��D?	�D?��D@	�D@��DA	�DA��DB	�DB��DC	�DC��DD	�DD��DE	�DE��DF DF��DG	�DG��DH	�DH��DI	�DI��DJ	�DJ��DK	�DK��DL	�DL��DM	�DM��DN	�DN��DO	�DO��DP	�DP��DQ	�DQ��DR	�DR��DS	�DS��DT	�DT��DU	�DU��DV	�DV��DW	�DW��DX	�DX��DY	�DY��DZ	�DZ��D[	�D[��D\	�D\��D]	�D]��D^	�D^��D_	�D_��D`	�D`��Da	�Da��Db	�Db��Dc	�Dc��Dd	�Dd��De	�De��Df	�Df��Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj��Dk	�Dk��Dl	�Dl��Dm	�Dm��Dn	�Dn��Do	�Do��Dp	�Dp��Dq	�Dq��Dr	�Dr��Ds	�Ds��Dt	�Dt|�Dy�4D�3D�Q�D��gD���D��D�^gD�� D���D� D�+3D���D���D�gD�D�D�x D��D� D�T�D� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AӴ9AӮAӮAӬAӴ9AӴ9AӸRAӺ^AӾwAӾwAӺ^Aӣ�A�7LAҼjAҋDA�`BA�$�A�JA��yA��A��;A���AѶFAѸRAѰ!A�A���A���A�$�A�^5A҃A�t�A�~�A��A��mA���A�ƨA�ZA�I�A�bAȾwAāA��/A�JA��mA�33A��hA��A�Q�A�~�A�%A��A���A��-A���A�l�A�~�A�(�A�E�A��9A�5?A�bNA��A���A���A�E�A�1'A��A���A�v�A��yA�A�A�^5A�5?A��;A�ȴA���A��^A��yA�oA�S�A��-A���A�A�A��uA��RA��A�bNA�VA�v�A���A�A��+A��HA��A���A�~�A�A�A�r�A��A�l�A�t�A�n�A��A�S�A�%A�oA}XA|5?Az�+Au��ArbAj=qAeVAbE�A` �A^ĜA]ƨA[G�AY7LAX^5AV�uAQ�AN^5AMx�AM7LAK
=AIAF��AES�AC��ABJA?�A=�hA<-A;�TA;��A;G�A:jA8r�A45?A3?}A2�\A1C�A0VA-��A,1'A*�!A(A�A&�9A$�9A#�A#�A#33A"v�A!p�A��A��A�A�A�uA�A��A�A�Ar�A�A�-A`BA�9AJA��AS�A�+A�A��A�A��A��A��A��A�hA
=A	|�A9XAhsAoA��A��A5?A%A�AM�A �/A   @��T@���@�=q@��/@��F@��@��9@� �@�
=@�7L@�Ĝ@�+@�@�o@��@�Ĝ@�|�@��@�R@�^5@�{@�^@�@�r�@���@��y@��`@�I�@�ƨ@ߍP@��@��@�Ĝ@�Q�@�l�@�ȴ@���@ٙ�@�O�@ؼj@�j@��/@�(�@�33@�v�@��@�`B@�p�@�x�@�O�@Դ9@�(�@�l�@��H@���@�1@�;d@���@θR@�M�@��#@�`B@�Ĝ@�Q�@˥�@ʧ�@��#@�X@�p�@�@�@�-@���@�O�@ȣ�@� �@�b@�C�@�~�@�n�@�@��@�z�@�Q�@�1'@���@�l�@�S�@�K�@��@�M�@��@���@���@�%@��
@���@�ff@�5?@�$�@���@���@��@�`B@��@��@�(�@��w@���@�o@���@�$�@��@�X@�/@���@�Q�@�  @���@�l�@���@��@��#@�`B@�/@��@�I�@�t�@��@��H@���@�ff@�V@�=q@�$�@���@��@�@�x�@�G�@��@���@�r�@�1@��F@�l�@�33@��@��!@��+@���@�@��7@��@�Z@� �@��
@��F@�|�@�;d@�"�@��y@���@���@���@�n�@�{@��T@���@�`B@�O�@�7L@���@�  @�
=@�
=@��@�M�@�-@�@��-@���@���@��h@�X@�G�@�&�@��@�Ĝ@��D@�bN@��@��P@�33@���@�5?@��@��^@���@�`B@�V@��/@���@�j@�I�@�1'@�  @�|�@��y@��!@��+@�n�@�^5@�V@�=q@�{@��^@�x�@�x�@�X@�&�@��@���@���@��@�j@� �@�1@��@��
@�ƨ@���@�dZ@�"�@�@��\@�$�@�-@��T@���@�p�@�X@��@��j@�bN@�  @���@�33@���@��@���@�=q@�@��@���@�hs@��@���@��u@��@��F@���@��P@�\)@��@��\@�E�@��@��@�7L@�%@�r�@�bN@�Z@�  @��;@��@�t�@�33@���@�M�@��@�@���@�x�@���@��@��9@�Z@�(�@��@�ƨ@���@�hs@�X@�w@w�@k"�@`b@U/@L��@D1@<��@9x�@1��@+�m@&v�@�w@��@ff@-@$�@��@�911111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   AӴ9AӮAӮAӬAӴ9AӴ9AӸRAӺ^AӾwAӾwAӺ^Aӣ�A�7LAҼjAҋDA�`BA�$�A�JA��yA��A��;A���AѶFAѸRAѰ!A�A���A���A�$�A�^5A҃A�t�A�~�A��A��mA���A�ƨA�ZA�I�A�bAȾwAāA��/A�JA��mA�33A��hA��A�Q�A�~�A�%A��A���A��-A���A�l�A�~�A�(�A�E�A��9A�5?A�bNA��A���A���A�E�A�1'A��A���A�v�A��yA�A�A�^5A�5?A��;A�ȴA���A��^A��yA�oA�S�A��-A���A�A�A��uA��RA��A�bNA�VA�v�A���A�A��+A��HA��A���A�~�A�A�A�r�A��A�l�A�t�A�n�A��A�S�A�%A�oA}XA|5?Az�+Au��ArbAj=qAeVAbE�A` �A^ĜA]ƨA[G�AY7LAX^5AV�uAQ�AN^5AMx�AM7LAK
=AIAF��AES�AC��ABJA?�A=�hA<-A;�TA;��A;G�A:jA8r�A45?A3?}A2�\A1C�A0VA-��A,1'A*�!A(A�A&�9A$�9A#�A#�A#33A"v�A!p�A��A��A�A�A�uA�A��A�A�Ar�A�A�-A`BA�9AJA��AS�A�+A�A��A�A��A��A��A��A�hA
=A	|�A9XAhsAoA��A��A5?A%A�AM�A �/A   @��T@���@�=q@��/@��F@��@��9@� �@�
=@�7L@�Ĝ@�+@�@�o@��@�Ĝ@�|�@��@�R@�^5@�{@�^@�@�r�@���@��y@��`@�I�@�ƨ@ߍP@��@��@�Ĝ@�Q�@�l�@�ȴ@���@ٙ�@�O�@ؼj@�j@��/@�(�@�33@�v�@��@�`B@�p�@�x�@�O�@Դ9@�(�@�l�@��H@���@�1@�;d@���@θR@�M�@��#@�`B@�Ĝ@�Q�@˥�@ʧ�@��#@�X@�p�@�@�@�-@���@�O�@ȣ�@� �@�b@�C�@�~�@�n�@�@��@�z�@�Q�@�1'@���@�l�@�S�@�K�@��@�M�@��@���@���@�%@��
@���@�ff@�5?@�$�@���@���@��@�`B@��@��@�(�@��w@���@�o@���@�$�@��@�X@�/@���@�Q�@�  @���@�l�@���@��@��#@�`B@�/@��@�I�@�t�@��@��H@���@�ff@�V@�=q@�$�@���@��@�@�x�@�G�@��@���@�r�@�1@��F@�l�@�33@��@��!@��+@���@�@��7@��@�Z@� �@��
@��F@�|�@�;d@�"�@��y@���@���@���@�n�@�{@��T@���@�`B@�O�@�7L@���@�  @�
=@�
=@��@�M�@�-@�@��-@���@���@��h@�X@�G�@�&�@��@�Ĝ@��D@�bN@��@��P@�33@���@�5?@��@��^@���@�`B@�V@��/@���@�j@�I�@�1'@�  @�|�@��y@��!@��+@�n�@�^5@�V@�=q@�{@��^@�x�@�x�@�X@�&�@��@���@���@��@�j@� �@�1@��@��
@�ƨ@���@�dZ@�"�@�@��\@�$�@�-@��T@���@�p�@�X@��@��j@�bN@�  @���@�33@���@��@���@�=q@�@��@���@�hs@��@���@��u@��@��F@���@��P@�\)@��@��\@�E�@��@��@�7L@�%@�r�@�bN@�Z@�  @��;@��@�t�@�33@���@�M�@��@�@���@�x�@���@��@��9@�Z@�(�@��@�ƨG�O�@�hs@�X@�w@w�@k"�@`b@U/@L��@D1@<��@9x�@1��@+�m@&v�@�w@��@ff@-@$�@��@�911111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
0!B
0!B
1'B
1'B
0!B
0!B
0!B
0!B
0!B
0!B
2-B
5?B
B�B
XB
aHB
jB
y�B
�VB
�VB
�VB
�{B
��B
��B
��B
��B
�B
�FB
�dB
�HB
��BBPB$�B�B�B �B#�B+B�BB
��B+B�B�B"�BaHB�\B{�BaHBdZBz�B�JB�JB�DB��B��B��B��BN�B�3B��B�5B�;B�#B��B�jB�-B��B�B�^B�dB�9B��B�oBx�Bs�BdZBW
BG�B8RB$�B%B�B��B��BbNBJ�B/B"�B�B
��B
�HB
�ZB
�B
�B
�/B
�HB
��B
ƨB
�}B
��B
��B
�%B
�B
r�B
H�B
\B	��B	�B	�B	�-B	��B	ffB	C�B	2-B	&�B	�B	�B	VB	B��B�B�;B�
B��B��B��BǮBɺB��B��B��B��B�B�B�
B�
B�B��B��B�B�HB�`B�`B�BB�B��BǮB��B�B�;B�ZB�TB�TB�BB�;B�#B�HB�sB�B�B�B�B�B�B�B�B�B�B�B��B��B��B�B�B�B�BB�B��B��BǮB��B��BŢBB��BBB��BBÖBÖBB��B�qB�jB�qB�wB�wB�wB�}BĜBĜBÖBǮB��B��B��B��B�B�)B�5B�ZB�fB�mB�sB�yB�B�B�B�B��B��B��B��B��B	B	B	B		7B	DB	
=B		7B	
=B	\B	uB	�B	 �B	'�B	)�B	,B	/B	2-B	33B	5?B	6FB	7LB	:^B	=qB	@�B	?}B	C�B	H�B	O�B	P�B	O�B	S�B	T�B	T�B	T�B	S�B	T�B	W
B	[#B	aHB	dZB	iyB	hsB	gmB	gmB	hsB	l�B	l�B	n�B	n�B	p�B	q�B	s�B	t�B	t�B	w�B	z�B	~�B	�B	�B	�B	�+B	�7B	�=B	�7B	�7B	�VB	�bB	�hB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�9B	�?B	�LB	�^B	�dB	�jB	�wB	�}B	�}B	�}B	��B	ÖB	ÖB	ĜB	ŢB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�/B	�5B	�;B	�BB	�;B	�;B	�;B	�BB	�BB	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�TB	�ZB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
	7B
{B
�B
�B
%�B
,B
5?B
=qB
D�B
K�B
M�B
VB
YB
^5B
dZB
iyB
m�B
q�B
u�B
x�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
0B
0B
1"B
1$B
0B
0B
0B
0B
0B
0B
2)B
59B
B�B
XB
aDB
jzB
y�B
�OB
�NB
�PB
�tB
�}B
�}B
��B
��B
��B
�?B
�ZB
�@B
��B
BEB$�B�B�B �B#�B*�B�BB
��B!B�B�B"�Ba;B�LB{�Ba<BdJBz�B�>B�=B�6B��B��B��B��BN�B�'B��B�(B�/B�B��B�`B�"B��B��B�SB�XB�/B��B�eBx�Bs�BdMBV�BG�B8DB$�BB�B��B��Bb@BJ�B/B"�B�B
��B
�;B
�MB
�B
��B
�"B
�;B
��B
ƚB
�pB
��B
��B
�B
�	B
r�B
H�B
RB	��B	�B	�B	�'B	��B	fbB	C�B	2-B	&�B	�B	�B	VB	B��B�B�=B�B��B��B��BǰBɾB��B��B��B��B�B�B�
B�B�B�B��B�B�KB�aB�aB�BB�B��BǭB��B�B�<B�[B�RB�UB�BB�<B�%B�HB�rB�}B�B�B�B�B�B�B�B�B�B�B��B��B��B�B�B�~B�CB�B��B��BǯB��B��BŢBB��BBB��BBÔBÖBB��B�pB�iB�pB�wB�yB�wB�~BĝBĞBÖBǬB��B��B��B��B�B�'B�4B�XB�cB�kB�pB�yB�~B�B�B�B��B��B��B��B��B	B	B	B		5B	>B	
7B		4B	
7B	XB	qB	�B	 �B	'�B	)�B	,B	/B	2&B	3.B	58B	6@B	7HB	:VB	=kB	@}B	?xB	C�B	H�B	O�B	P�B	O�B	S�B	T�B	T�B	T�B	S�B	T�B	WB	[B	aAB	dRB	inB	hpB	gbB	gdB	hkB	l�B	l�B	n�B	n�B	p�B	q�B	s�B	t�B	t�B	w�B	z�B	~�B	�B	�B	�B	�#B	�.B	�6B	�-B	�0B	�MB	�ZB	�^B	�^B	�fB	�mB	�rB	�wB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�+B	�,B	�2B	�DB	�TB	�ZB	�_B	�mB	�rB	�pB	�rB	�wB	ËB	ÍB	ďB	ŖB	ƞB	ƚB	ǤB	ǤB	ǤB	ǥB	ɲB	˻B	˾B	̿B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�	B	�	B	�
B	�
B	�B	�B	�B	�B	�B	�B	�"B	�*B	�0B	�6B	�0B	�0B	�-B	�5B	�5B	�6B	�;B	�;B	�BB	�AB	�IB	�IB	�HB	�MB	�[B	�YB	�aB	�bB	�`B	�aB	�bB	�_B	�fB	�mB	�jB	�rB	�yB	�yB	�zB	�zB	�xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
B
B
B
B
B
B
B
B
B
 B
B
"G�O�B
	(B
mB
�B
�B
%�B
+�B
5/B
=cB
D�B
K�B
M�B
U�B
Y	B
^&B
dJB
ijB
m�B
q�B
u�B
x�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.15 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436302016080714363020160807143630  AO  ARCAADJP                                                                    20150226221339    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221339  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221339  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143630  IP                  G�O�G�O�G�O�                