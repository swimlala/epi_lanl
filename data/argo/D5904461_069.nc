CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-09-01T02:16:21Z AOML 3.0 creation; 2016-08-07T21:36:38Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150901021621  20160807143638  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               EA   AO  5286_8897_069                   2C  D   APEX                            6531                            072314                          846 @�k�x���1   @�k���@4o��-V�cB��n�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    EA   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0fD0�fD1  D1� D2  D2� D3  D3� D4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�DyffD��D�9�D�|�D���D�	�D�9�D�s3D���D�fD�@ D�|�DǬ�D�	�D�<�D�p D���D��D�33D�|�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@���AffA"ffABffAbffA�33A�33A�33A�33A�33A�33A�33A�33B ��B��B��B��B ��B(��B1  B9  B@��BH��BP��BX��B`��Bh��Bp��Bx��B��B�L�B�L�B�L�B�L�B�L�B�� B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C &fC&fC&fC&fC&fC
&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC &fC"&fC$&fC&&fC(&fC*&fC,&fC.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd&fCf&fCh&fCj&fCl&fCn&fCp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D ��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D		�D	��D
	�D
��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D 	�D ��D!	�D!��D"	�D"��D#	�D#��D$	�D$��D%	�D%��D&	�D&��D'	�D'��D(	�D(��D)	�D)��D*	�D*��D+	�D+��D,	�D,��D-	�D-��D.	�D.��D/	�D/��D0 D0� D1	�D1��D2	�D2��D3	�D3��D4	�D4��D5 D5��D6	�D6��D7	�D7��D8	�D8��D9	�D9��D:	�D:��D;	�D;��D<	�D<��D=	�D=��D>	�D>��D?	�D?��D@	�D@��DA	�DA��DB	�DB��DC	�DC��DD	�DD��DE	�DE��DF	�DF��DG	�DG��DH	�DH��DI	�DI��DJ DJ� DK	�DK��DL	�DL��DM	�DM��DN	�DN��DO	�DO��DP	�DP��DQ	�DQ��DR	�DR��DS	�DS��DT	�DT��DU	�DU��DV	�DV��DW	�DW��DX	�DX��DY	�DY��DZ	�DZ��D[	�D[��D\	�D\��D]	�D]��D^	�D^��D_	�D_��D`	�D`��Da	�Da��Db	�Db��Dc	�Dc��Dd	�Dd��De	�De��Df	�Df��Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj��Dk	�Dk��Dl	�Dl��Dm	�Dm��Dn	�Dn��Do	�Do��Dp	�Dp��Dq	�Dq��Dr	�Dr��Ds	�Ds��Dt	�Dt�4Dyp D�!�D�>gD���D���D�gD�>gD�x D���D�3D�D�D���DǱ�D�gD�A�D�t�D�њD��D�8 D�D��g11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1'A�+A��A�%A��;A��/A�ȴA��A�~�A��#A�-A��A�-A�A��A�1A��+A�VAߏ\A�/Aޏ\A�C�A��A�A݋DA�~�A��
A�v�A���A�
=A��yAΙ�A͑hA�A�7LA�p�A��;A�
=A��A���Aǥ�A��A�  A��`A�^5A�v�A�ZA�ĜA�A��wA���A��DA�XA���A�`BA���A��RA�ȴA�ffA��jA��TA�9XA�bA�x�A�"�A��
A�33A��A�ĜA���A��jA��hA��A�C�A��`A�hsA�1'A�l�A���A��!A�~�A��A�+A�l�A�A��FA���A�7LA��/A�ȴA�&�A���A��A���A��A�S�A�(�A�r�A��FA���A��A��A���A�l�A�ZA�5?A��A�`BA�7LA���A�p�A��A�A|��A{%Ay+Aw�FAv=qAu`BAsx�Anv�An$�Al�HAi`BAf9XAb�A`1A_33A]�A\I�A[%AY�AW|�AT~�AR�9AP{AM�mALjAJ��AG�ADffAA�-A?��A>�RA=t�A<-A;��A;\)A;A9t�A8E�A7��A7?}A7`BA7�hA6��A69XA5��A3x�A0n�A/��A/�^A/C�A.�`A.ZA,�`A)�wA'��A&�yA&�A$bNA$1'A"�yA"-A z�A��A ��A jA ��A �uA��AjA�7A��A�#A�A�!A�A�AJA�mAbNA1'A��A��AhsAdZAp�A;dA �A�A/A��A�HA�-A�A/AVA
jA	��A��A��A�wA1A��A��A�A+@�`B@�7L@�~�@���@���@��!@�p�@��u@�-@��@�33@�O�@�r�@�S�@�~�@�|�@��@�O�@��m@�K�@��@�F@�@�C�@�ȴ@�u@�33@�n�@�X@��@��y@��@�^5@ݑh@ݙ�@�`B@�O�@�G�@��@���@�(�@��;@�33@ڏ\@�&�@�ȴ@թ�@��`@ҟ�@�7L@�  @�+@�5?@͙�@�X@�$�@���@���@�9X@�z�@��@�S�@�\)@ˍP@���@˥�@�;d@�;d@�@���@���@��@ə�@�v�@ʇ+@ȓu@�5?@��#@�7L@ļj@�A�@Õ�@�1'@å�@��D@��/@�J@��@��R@�$�@�@�v�@��#@��m@���@�x�@��9@�Q�@�Ĝ@�"�@��@���@���@��h@��7@��@�t�@�
=@��\@��@�@�~�@�M�@�$�@���@�~�@�{@�I�@��F@���@�|�@�ƨ@��h@���@���@�o@��@�b@�|�@���@�v�@�hs@��D@�b@�1'@�(�@��!@��@���@�j@���@��@���@�9X@��u@���@�/@�Ĝ@��u@�"�@��T@���@�"�@��P@�C�@��;@�9X@�9X@���@�|�@��@���@�j@���@�?}@��@���@�|�@��
@�(�@�j@�%@���@�x�@���@��@�?}@��j@�(�@��w@�K�@��@�V@���@�hs@��@��`@��u@��@�\)@�C�@���@���@�5?@�7L@�%@��@��j@��@�A�@��@��;@��@��
@�\)@��y@�V@�J@��@���@���@�x�@�G�@�&�@���@���@�j@�Q�@�1'@��@��P@�|�@�t�@�|�@�l�@�
=@�^5@��-@���@���@��7@�hs@�`B@�X@�G�@���@��@�1'@�l�@��\@��^@���@��@�7L@��j@�(�@��
@���@�
=@���@��9@���@���@��H@�"�@���@��!@���@��!@�~�@��+@���@�n�@�@��T@��T@��T@��T@��#@���@��w@��7@��^@|�D@y�7@j��@a&�@\j@W�@Q�@L9X@E�-@:^5@1�@-p�@'K�@  �@��@{@%@K�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�1'A�+A��A�%A��;A��/A�ȴA��A�~�A��#A�-A��A�-A�A��A�1A��+A�VAߏ\A�/Aޏ\A�C�A��A�A݋DA�~�A��
A�v�A���A�
=A��yAΙ�A͑hA�A�7LA�p�A��;A�
=A��A���Aǥ�A��A�  A��`A�^5A�v�A�ZA�ĜA�A��wA���A��DA�XA���A�`BA���A��RA�ȴA�ffA��jA��TA�9XA�bA�x�A�"�A��
A�33A��A�ĜA���A��jA��hA��A�C�A��`A�hsA�1'A�l�A���A��!A�~�A��A�+A�l�A�A��FA���A�7LA��/A�ȴA�&�A���A��A���A��A�S�A�(�A�r�A��FA���A��A��A���A�l�A�ZA�5?A��A�`BA�7LA���A�p�A��A�A|��A{%Ay+Aw�FAv=qAu`BAsx�Anv�An$�Al�HAi`BAf9XAb�A`1A_33A]�A\I�A[%AY�AW|�AT~�AR�9AP{AM�mALjAJ��AG�ADffAA�-A?��A>�RA=t�A<-A;��A;\)A;A9t�A8E�A7��A7?}A7`BA7�hA6��A69XA5��A3x�A0n�A/��A/�^A/C�A.�`A.ZA,�`A)�wA'��A&�yA&�A$bNA$1'A"�yA"-A z�A��A ��A jA ��A �uA��AjA�7A��A�#A�A�!A�A�AJA�mAbNA1'A��A��AhsAdZAp�A;dA �A�A/A��A�HA�-A�A/AVA
jA	��A��A��A�wA1A��A��A�A+@�`B@�7L@�~�@���@���@��!@�p�@��u@�-@��@�33@�O�@�r�@�S�@�~�@�|�@��@�O�@��m@�K�@��@�F@�@�C�@�ȴ@�u@�33@�n�@�X@��@��y@��@�^5@ݑh@ݙ�@�`B@�O�@�G�@��@���@�(�@��;@�33@ڏ\@�&�@�ȴ@թ�@��`@ҟ�@�7L@�  @�+@�5?@͙�@�X@�$�@���@���@�9X@�z�@��@�S�@�\)@ˍP@���@˥�@�;d@�;d@�@���@���@��@ə�@�v�@ʇ+@ȓu@�5?@��#@�7L@ļj@�A�@Õ�@�1'@å�@��D@��/@�J@��@��R@�$�@�@�v�@��#@��m@���@�x�@��9@�Q�@�Ĝ@�"�@��@���@���@��h@��7@��@�t�@�
=@��\@��@�@�~�@�M�@�$�@���@�~�@�{@�I�@��F@���@�|�@�ƨ@��h@���@���@�o@��@�b@�|�@���@�v�@�hs@��D@�b@�1'@�(�@��!@��@���@�j@���@��@���@�9X@��u@���@�/@�Ĝ@��u@�"�@��T@���@�"�@��P@�C�@��;@�9X@�9X@���@�|�@��@���@�j@���@�?}@��@���@�|�@��
@�(�@�j@�%@���@�x�@���@��@�?}@��j@�(�@��w@�K�@��@�V@���@�hs@��@��`@��u@��@�\)@�C�@���@���@�5?@�7L@�%@��@��j@��@�A�@��@��;@��@��
@�\)@��y@�V@�J@��@���@���@�x�@�G�@�&�@���@���@�j@�Q�@�1'@��@��P@�|�@�t�@�|�@�l�@�
=@�^5@��-@���@���@��7@�hs@�`B@�X@�G�@���@��@�1'@�l�@��\@��^@���@��@�7L@��j@�(�@��
@���@�
=@���@��9@���@���@��H@�"�@���@��!@���@��!@�~�@��+@���@�n�@�@��T@��T@��T@��T@��#G�O�@��w@��7@��^@|�D@y�7@j��@a&�@\j@W�@Q�@L9X@E�-@:^5@1�@-p�@'K�@  �@��@{@%@K�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�hB	�hB	�bB	�hB	�oB	�oB	�uB	��B	��B
$�B
~�B
�1B
��B
��B
�3B
��B
�HB
�BB
�B
��B
�B
�B
�BB
�BB
�;B
�ZB
�ZB
�yB  B'�BXB�wB��B�HB�BB�B/B/B7LBZBgmB{�B��B�uB�Bt�B�B��B�B�B�!B�B�B�dB�wB��B��B�oB�DB�Bz�BaHB^5B~�B��B��B�3B��B��B��BB�dB�^B�LB�!B��B��B�Bn�Bq�Bm�BH�B33B��B�NB+B<jB5?BB�Bq�BjBcTBN�B(�B��B�TB�wB�B��Bs�BVB@�B&�BPB
�/B
��B
�VB
v�B
O�B
<jB
2-B
�B
	7B	��B	�B	�;B	��B	ɺB	�jB	��B	��B	�\B	u�B	_;B	D�B	7LB	0!B	&�B	�B	hB	B��B�B�BB�B��BǮB��B�dB�FB�9B�B��B��B��B��B�{B�oB�JB�uB��B�FB�}BÖB��B��BɺB��B�XB�qBǮB�B�)B�#B��B��BȴBɺB��B��B��B�#B�ZB�ZB�ZB	B	+B	'�B	1'B	1'B	,B	0!B	?}B	B�B	E�B	A�B	8RB	>wB	9XB	B�B	O�B	P�B	T�B	P�B	H�B	L�B	M�B	N�B	P�B	J�B	K�B	L�B	L�B	D�B	;dB	8RB	=qB	&�B	"�B	�B	�B	 �B	,B	/B	)�B	JB��B�HB��B��B�B�B�B�B�B�B�B�fB�`B�sB�`B�ZB��B��B�B��B��B	B	{B	�B	%�B	$�B	�B	�B	�B	{B	\B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	%�B	,B	/B	-B	)�B	&�B	"�B	 �B	�B	�B	�B	{B	{B	�B	�B	�B	"�B	/B	-B	.B	49B	33B	2-B	6FB	<jB	@�B	F�B	G�B	G�B	F�B	C�B	F�B	L�B	T�B	[#B	^5B	ZB	T�B	S�B	W
B	[#B	[#B	ZB	bNB	cTB	ZB	]/B	e`B	k�B	bNB	`BB	aHB	dZB	w�B	q�B	p�B	l�B	jB	iyB	o�B	iyB	cTB	dZB	gmB	l�B	�+B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�XB	�RB	�3B	�LB	�LB	��B	��B	��B	�{B	�DB	�7B	�7B	�1B	�=B	�1B	�1B	�1B	�7B	�7B	�B	� B	�B	�7B	�1B	�PB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�FB	�^B	��B	ŢB	��B	�B	�B	�#B	�/B	�;B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
  B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
DB
�B
uB
(�B
$�B
(�B
/B
5?B
6FB
=qB
F�B
I�B
P�B
T�B
[#B
aHB
dZB
hsB
m�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	�iB	�kB	�dB	�kB	�tB	�qB	�yB	��B	��B
$�B
~�B
�,B
��B
��B
�(B
��B
�@B
�;B
��B
��B
� B
�B
�6B
�9B
�1B
�PB
�RB
�qB
��B'�BXB�lB��B�9B�B �B�B/B/B7?BZBgaB{�B��B�kB�Bt�B��B��B�B�	B�B�B�B�WB�lB��B�{B�`B�8B�Bz�Ba9B^'B~�B��B��B�)BʶB˽B˺BB�WB�UB�<B�B��B��B�Bn�Bq�Bm�BH�B3%B��B�@BB<aB53BB�Bq�BjtBcFBN�B(�B��B�GB�gB��B�rBs�BU�B@vB&�BBB
�#B
��B
�LB
v�B
O�B
<_B
2%B
�B
	0B	��B	�B	�0B	��B	ɳB	�cB	��B	��B	�XB	u�B	_8B	D�B	7KB	0B	&�B	�B	iB	!B��B�~B�FB�B��BǰB��B�hB�IB�<B�B��B��B��B��B��B�tB�NB�zB��B�KB�BÙB��B��BɻB��B�ZB�rBǭB�B�)B�#B� B��BȶBɼB��B��B��B�#B�XB�[B�ZB	
B	)B	'�B	1"B	1$B	,B	0B	?zB	B�B	E�B	A�B	8NB	>tB	9RB	B�B	O�B	P�B	T�B	P�B	H�B	L�B	M�B	N�B	P�B	J�B	K�B	L�B	L�B	D�B	;`B	8OB	=nB	&�B	"�B	�B	�B	 �B	,B	/B	)�B	EB��B�FB��B��B�}B�B�B�B�B�B�}B�cB�]B�qB�\B�YB��B��B�B��B��B	B	wB	�B	%�B	$�B	�B	�B	�B	vB	YB	zB	�B	�B	�B	�B	�B	�B	 �B	!�B	%�B	,B	/B	-B	)�B	&�B	"�B	 �B	�B	�B	�B	wB	yB	�B	�B	�B	"�B	/B	-
B	.B	41B	3+B	2&B	6@B	<dB	@}B	F�B	G�B	G�B	F�B	C�B	F�B	L�B	T�B	[B	^-B	ZB	T�B	S�B	WB	[B	[B	ZB	bHB	cMB	ZB	])B	eXB	k|B	bFB	`8B	a@B	dRB	w�B	q�B	p�B	l�B	jvB	iqB	o�B	iqB	cKB	dRB	gdB	l�B	�"B	�]B	�qB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�KB	�JB	�)B	�AB	�BB	��B	��B	��B	�sB	�;B	�/B	�,B	�'B	�5B	�'B	�'B	�*B	�.B	�.B	�B	�B	�B	�-B	�'B	�FB	�^B	�kB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�	B	�B	�B	�!B	�9B	�UB	�B	ŗB	��B	��B	�B	�B	�%B	�/B	�XB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�}B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B
9B
rB
eB
(�B
$�B
(�B
/B
53B
67B
=dB
F�B
I�B
P�B
T�B
[B
a7B
dIB
hcB
m�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.15 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436382016080714363820160807143638  AO  ARCAADJP                                                                    20150901021621    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150901021621  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150901021621  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143638  IP                  G�O�G�O�G�O�                