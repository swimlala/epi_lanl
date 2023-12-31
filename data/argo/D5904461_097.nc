CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-01-26T20:17:16Z AOML 3.0 creation; 2016-08-07T21:36:43Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160126201716  20160807143643  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               aA   AO  5286_8897_097                   2C  D   APEX                            6531                            072314                          846 @א�d���1   @א���ӊ@3$�t�j�c.�G�{1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    aA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C33C	�fC�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtS3Dy  D���D�FfD�� D��3D���D�)�D�l�D��3D��D�I�D�s3D��fD���D�0 Dڠ D���D�  D�33D� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���AffA"ffABffAbffA�33A�33A�33A�33A�33A�33A�33A�33B ��B34B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B� B�L�B�L�C &fC&fC&fC&fCY�C
�C�C&fC&fC&fC&fC&fC&fC&fC&fC&fC &fC"&fC$&fC&&fC(&fC*&fC,&fC.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd&fCf&fCh&fCj&fCl&fCn&fCp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D ��D	�D��D4D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D		�D	��D
	�D
��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D 	�D ��D!	�D!��D"	�D"��D#	�D#��D$	�D$��D%	�D%��D&	�D&��D'	�D'��D(	�D(��D)	�D)��D*	�D*��D+	�D+��D,	�D,��D-	�D-��D.	�D.��D/	�D/��D0	�D0��D1	�D1��D2	�D2��D3	�D3��D4	�D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8��D9	�D9��D:	�D:��D;	�D;��D<	�D<��D=	�D=��D>	�D>��D?	�D?��D@	�D@��DA	�DA��DB	�DB��DC	�DC��DD	�DD��DE	�DE��DF	�DF��DG	�DG��DH	�DH��DI	�DI��DJ	�DJ��DK	�DK��DL	�DL��DM	�DM��DN	�DN��DO	�DO��DP	�DP��DQ	�DQ��DR	�DR��DS	�DS��DT	�DT��DU	�DU��DV	�DV��DW	�DW��DX	�DX��DY	�DY��DZ	�DZ��D[	�D[��D\	�D\��D]	�D]��D^	�D^��D_	�D_��D`	�D`��Da	�Da��Db	�Db��Dc	�Dc��Dd	�Dd��De	�De��Df	�Df��Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj��Dk	�Dk��Dl	�Dl��Dm	�Dm��Dn	�Dn��Do	�Do��Dp	�Dp��Dq	�Dq��Dr	�Dr��Ds	�Ds��Dt	�Dt\�Dy	�D��D�K3D���D�� D��D�.gD�q�D�� D�!�D�NgD�x D��3D��gD�4�Dڤ�D�њD��D�8 D��D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A��
A���A��A��#A��HA��TA��TA��TA��TA��TA��`A��yA��mA��TA��mA��TA��yA���A�  A�%A�A�JA�{A��A�VA�  A���A��A��`A��HA��#A��A��
A���A���Aɧ�A��Aȇ+A�K�A�1AǑhA�I�A�{A���A���AƧ�AƇ+A�t�A�dZA�XA�G�A�1'A��A���A��#A���AŮA�Q�A��#A��
A�A�hsA�z�A�JA��A��+A���A��/A�9XA�7LA�VA��/A�t�A�A�/A��+A���A�jA�bA��A��A�bNA�1A��A�|�A���A��A�I�A�"�A���A�S�A���A��A��/A�+A�ZA�VA~��A{�Ax��Au`BAr��Aq�-Aq"�Ao��An�Al��Aj�uAe�AbbNA_�A]�AY�AVAR�jAQ�ANv�AJ9XAD�/AB�uA@�HA?K�A=A<��A:�+A9�-A7��A6bA5S�A4��A4ZA3��A3"�A1�TA0�`A/+A-XA+?}A)��A(1A&I�A%S�A$�!A"�jA!��A 9XA��AȴA(�A&�A�mA��AbNAjA��A�+A�wA�9A�AG�AĜA��A��A��A��A
A	��A��A1A��AK�A��A��A;dA9XA�
A�PAK�A�-AK�A 1'@�C�@�ff@���@�{@�bN@��\@�9X@���@���@�D@�D@�Q�@�9X@� �@��m@�@��D@��^@�@��@ꟾ@���@��#@陚@�p�@�@�S�@���@�@���@��@��@�x�@�@أ�@�?}@�@ى7@��/@�Z@׾w@�v�@�v�@���@� �@�dZ@�K�@�K�@�t�@�"�@ҟ�@с@���@̴9@�Q�@� �@�\)@��@ʸR@�{@ȼj@�J@��@�?}@��@�I�@��@�dZ@�o@��+@��-@��@��9@�9X@��@�v�@�@�x�@�r�@��H@�~�@�-@���@���@�`B@��`@��9@�A�@��@��P@��@���@�n�@��@���@���@�I�@��
@�|�@��@��+@��@��@��@�|�@��@���@�n�@���@���@�hs@��/@�l�@�n�@��@��^@���@���@�p�@��@��@��
@�33@�@��@�n�@���@��7@�X@�Ĝ@��@�A�@���@���@��;@�o@��R@���@���@�~�@�v�@�V@�M�@���@��H@��H@��y@��y@��y@��y@��y@��@��H@�
=@���@���@��@�E�@�E�@�=q@�-@��#@�p�@���@�(�@�b@�1@��@��F@�l�@�+@�
=@���@�~�@�E�@��^@���@�p�@���@�Ĝ@��u@�j@�A�@�  @���@���@�\)@�K�@�;d@�"�@�o@�v�@��@��h@��@��`@��@���@���@��9@�A�@�(�@���@�ƨ@�l�@�S�@�+@�o@���@���@�p�@�`B@�7L@��@��/@�Ĝ@��@���@�z�@�9X@� �@���@���@��w@��w@�l�@�ff@��^@�G�@�%@��`@��9@�Z@��w@�;d@��@�ȴ@���@��\@��R@��\@�^5@��#@��@�?}@�&�@���@��9@�j@��@��;@��F@��@�dZ@�C�@�
=@��@��R@���@��+@�ff@�-@��@�@�hs@�hs@�/@��/@�Ĝ@��9@�bN@��@��w@�o@��@��!@���@���@��+@�ff@�M�@��@�x�@�?}@���@��u@�9X@��@K�@~��@}��@}p�@}V@|z�@|(�@{�
@{��@{o@zM�@y�7@yx�@yX@y%@tz�@nE�@h��@_��@X�@P��@JM�@Ct�@:�!@5V@/+@(��@"��@��@hs@�@�!@E�@��@Q�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A���A���A���A���A��
A���A��A��#A��HA��TA��TA��TA��TA��TA��`A��yA��mA��TA��mA��TA��yA���A�  A�%A�A�JA�{A��A�VA�  A���A��A��`A��HA��#A��A��
A���A���Aɧ�A��Aȇ+A�K�A�1AǑhA�I�A�{A���A���AƧ�AƇ+A�t�A�dZA�XA�G�A�1'A��A���A��#A���AŮA�Q�A��#A��
A�A�hsA�z�A�JA��A��+A���A��/A�9XA�7LA�VA��/A�t�A�A�/A��+A���A�jA�bA��A��A�bNA�1A��A�|�A���A��A�I�A�"�A���A�S�A���A��A��/A�+A�ZA�VA~��A{�Ax��Au`BAr��Aq�-Aq"�Ao��An�Al��Aj�uAe�AbbNA_�A]�AY�AVAR�jAQ�ANv�AJ9XAD�/AB�uA@�HA?K�A=A<��A:�+A9�-A7��A6bA5S�A4��A4ZA3��A3"�A1�TA0�`A/+A-XA+?}A)��A(1A&I�A%S�A$�!A"�jA!��A 9XA��AȴA(�A&�A�mA��AbNAjA��A�+A�wA�9A�AG�AĜA��A��A��A��A
A	��A��A1A��AK�A��A��A;dA9XA�
A�PAK�A�-AK�A 1'@�C�@�ff@���@�{@�bN@��\@�9X@���@���@�D@�D@�Q�@�9X@� �@��m@�@��D@��^@�@��@ꟾ@���@��#@陚@�p�@�@�S�@���@�@���@��@��@�x�@�@أ�@�?}@�@ى7@��/@�Z@׾w@�v�@�v�@���@� �@�dZ@�K�@�K�@�t�@�"�@ҟ�@с@���@̴9@�Q�@� �@�\)@��@ʸR@�{@ȼj@�J@��@�?}@��@�I�@��@�dZ@�o@��+@��-@��@��9@�9X@��@�v�@�@�x�@�r�@��H@�~�@�-@���@���@�`B@��`@��9@�A�@��@��P@��@���@�n�@��@���@���@�I�@��
@�|�@��@��+@��@��@��@�|�@��@���@�n�@���@���@�hs@��/@�l�@�n�@��@��^@���@���@�p�@��@��@��
@�33@�@��@�n�@���@��7@�X@�Ĝ@��@�A�@���@���@��;@�o@��R@���@���@�~�@�v�@�V@�M�@���@��H@��H@��y@��y@��y@��y@��y@��@��H@�
=@���@���@��@�E�@�E�@�=q@�-@��#@�p�@���@�(�@�b@�1@��@��F@�l�@�+@�
=@���@�~�@�E�@��^@���@�p�@���@�Ĝ@��u@�j@�A�@�  @���@���@�\)@�K�@�;d@�"�@�o@�v�@��@��h@��@��`@��@���@���@��9@�A�@�(�@���@�ƨ@�l�@�S�@�+@�o@���@���@�p�@�`B@�7L@��@��/@�Ĝ@��@���@�z�@�9X@� �@���@���@��w@��w@�l�@�ff@��^@�G�@�%@��`@��9@�Z@��w@�;d@��@�ȴ@���@��\@��R@��\@�^5@��#@��@�?}@�&�@���@��9@�j@��@��;@��F@��@�dZ@�C�@�
=@��@��R@���@��+@�ff@�-@��@�@�hs@�hs@�/@��/@�Ĝ@��9@�bN@��@��w@�o@��@��!@���@���@��+@�ff@�M�@��@�x�@�?}@���@��u@�9X@��@K�@~��@}��@}p�@}V@|z�@|(�@{�
@{��@{o@zM�@y�7@yx�@yXG�O�@tz�@nE�@h��@_��@X�@P��@JM�@Ct�@:�!@5V@/+@(��@"��@��@hs@�@�!@E�@��@Q�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
\)B
\)B
[#B
\)B
\)B
\)B
]/B
]/B
_;B
_;B
_;B
^5B
^5B
^5B
^5B
_;B
_;B
^5B
^5B
^5B
`BB
e`B
hsB
o�B
�B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�-B
��B�B;dBC�BL�Bk�B�JB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�jB��B�#B�BhB<jBH�B5?B�B��B��B��B��B��B��B�hB�JB}�B}�B�oBv�BQ�BF�B+B�B�BĜB��BgmBP�B@�B+B
�B
�fB
�
B
��B
��B
u�B
J�B
B	ɺB	�-B	��B	�hB	� B	x�B	r�B	hsB	]/B	M�B	5?B	VB�B�ZB�BÖB�!B��B��B�7Bt�BbNB]/B[#BZBXBW
BXBW
BXB[#B\)B]/B]/B\)B]/B`BB`BBbNBjBm�Bp�Bv�Bz�B{�B{�B�B�DB��B��B��B��B��B��B��B�\B�Bx�Bo�BjBhsBffBdZBbNBaHB`BB^5B]/B]/B^5B`BBdZBe`BgmBl�Bm�Bp�Bq�Bo�Bm�Bk�Bk�BhsBk�Bm�Bm�Bn�Bu�Bu�Bu�Bv�Bw�B~�B�7B�VB�\B�\B�VB�VB�oB�hB�\B�PB�JB�DB�=B�=B�7B�1B�%B�%B�B�B{�Bx�Bw�Bu�Bq�Bv�B~�B�%B�=B�bB�hB��B�{B��B��B��B��B��B��B�B�-B�-B�!B�B�B�B�B�B�!B�-B�3B�'B�B�B�B�B��B��B�B�B�!B�?B�XB�^B�wBĜBǮBǮBɺB��B��B��B��B�B�B�B�)B�/B�5B�;B�HB�fB�fB�sB�B�B�B�B��B��B	B	B		7B	PB	hB	�B	�B	�B	�B	 �B	"�B	)�B	+B	.B	8RB	>wB	?}B	@�B	@�B	B�B	E�B	H�B	O�B	T�B	VB	W
B	]/B	_;B	cTB	dZB	hsB	k�B	m�B	p�B	s�B	t�B	x�B	|�B	}�B	~�B	� B	�B	�B	�B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�9B	�?B	�FB	�LB	�qB	ĜB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�B	�B	�#B	�#B	�)B	�/B	�)B	�5B	�BB	�BB	�NB	�TB	�NB	�NB	�TB	�`B	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
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
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
+B
+B
%B
+B
+B
+B
1B
1B
	7B

=B

=B

=B
DB
JB
JB
PB
PB
VB
VB
VB
VB
\B
\B
\B
{B
oB
�B
�B
&�B
0!B
6FB
=qB
B�B
H�B
N�B
T�B
ZB
^5B
bNB
gmB
l�B
o�B
s�B
v�B
z�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
\"B
\$B
[B
\ B
\"B
\$B
])B
]&B
_4B
_5B
_3B
^/B
^0B
^0B
^.B
_3B
_4B
^/B
^-B
^/B
`=B
eZB
hlB
o�B
�B
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
�B
�&B
��BuB;WBC�BL�BkyB�<B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�[B��B�B�B\B<aBH�B53B�B��B��B��B��B��B�sB�[B�<B}�B}�B�aBv�BQ�BF�B*�B�B�BčB��Bg`BP�B@wB B
�B
�WB
��B
�vB
��B
u�B
J�B
B	ɶB	�*B	��B	�eB	�B	x�B	r�B	hsB	],B	M�B	5?B	WB�B�\B�"BÜB�%B��B��B�=Bt�BbUB]5B[,BZ%BW�BWBXBWBXB[.B\/B]4B]7B\2B]5B`KB`IBbSBj�Bm�Bp�Bv�Bz�B{�B{�B�
B�IB��B��B��B��B��B��B��B�`B�Bx�Bo�Bj�BhzBflBd`BbTBaLB`HB^:B]5B]1B^<B`GBd\BedBgrBl�Bm�Bp�Bq�Bo�Bm�Bk�Bk�BhyBk�Bm�Bm�Bn�Bu�Bu�Bu�Bv�Bw�B~�B�8B�XB�^B�^B�VB�XB�pB�jB�^B�QB�MB�GB�@B�AB�:B�5B�$B�$B�"B� B{�Bx�Bw�Bu�Bq�Bv�B B�(B�AB�dB�jB��B�{B��B��B��B��B��B��B�B�-B�-B�#B�B�B�B�B�B�!B�-B�1B�&B�B�B�B�B��B��B�B�B� B�?B�UB�\B�uBĚBǫBǪBɸB��B��B��B��B�B�B�B�$B�-B�3B�8B�FB�eB�eB�nB�}B�B�B�B��B��B	B	B		3B	KB	bB	�B	�B	�B	�B	 �B	"�B	)�B	*�B	.B	8KB	>pB	?vB	@}B	@}B	B�B	E�B	H�B	O�B	T�B	U�B	WB	](B	_1B	cMB	dQB	hjB	k�B	m�B	p�B	s�B	t�B	x�B	|�B	}�B	~�B	�B	�B	�B	�B	�LB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�5B	�;B	�BB	�fB	ĒB	ǢB	ȩB	ʶB	ʷB	ʶB	ʷB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�B	�)B	�5B	�6B	�@B	�HB	�AB	�CB	�GB	�UB	�bB	�bB	�iB	�rB	�yB	�B	�~B	�B	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
 B
 B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
#B
&B
	&B

1B

-B

0B
6B
<B
?B
CB
?B
HB
IB
IB
IB
LB
NB
NG�O�B
aB
�B
�B
&�B
0B
68B
=eB
B�B
H�B
N�B
T�B
ZB
^#B
b?B
g]B
l{B
o�B
s�B
v�B
z�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.15 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436432016080714364320160807143643  AO  ARCAADJP                                                                    20160126201716    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160126201716  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160126201716  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143643  IP                  G�O�G�O�G�O�                