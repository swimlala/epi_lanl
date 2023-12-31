CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-07-24T05:01:45Z AOML 3.0 creation; 2016-08-07T21:36:48Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160724050145  20160825183352  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5286_8897_131                   2C  D   APEX                            6531                            072314                          846 @׽�8R
�1   @׽����&@5���n��c;dZ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'y�D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD� D�9�D�ffD�ɚD�	�D�I�D�� D���D�3D�C3D�y�D�� D� D�<�Dډ�D��fD���D�6fD�p D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���AffA"ffABffAbffA�33A�33A�33A�33A�33A�33A�33A�33B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�� B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B؀ B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C &fC&fC&fC&fC&fC
&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC &fC"&fC$&fC&&fC(&fC*&fC,&fC.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd&fCf&fCh&fCj&fCl&fCn&fCp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D ��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D		�D	��D
	�D
��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D 	�D ��D!	�D!��D"	�D"��D#	�D#��D$	�D$��D%	�D%��D&	�D&��D'	�D'�4D(	�D(��D)	�D)��D*	�D*��D+	�D+��D,	�D,��D-	�D-��D.	�D.��D/	�D/��D0	�D0��D1	�D1��D2	�D2��D3	�D3��D4	�D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8��D9	�D9��D:	�D:��D;	�D;��D<	�D<��D=	�D=��D>	�D>��D?	�D?��D@	�D@��DA	�DA��DB	�DB��DC	�DC��DD	�DD��DE	�DE��DF	�DF��DG	�DG��DH	�DH��DI	�DI��DJ	�DJ��DK	�DK��DL	�DL��DM	�DM��DN	�DN��DO	�DO��DP	�DP��DQ	�DQ��DR	�DR��DS	�DS��DT	�DT��DU	�DU��DV	�DV��DW	�DW��DX	�DX��DY	�DY��DZ	�DZ��D[	�D[��D\	�D\��D]	�D]��D^	�D^��D_	�D_��D`	�D`��Da	�Da��Db	�Db��Dc	�Dc��Dd	�Dd��De	�De��Df	�Df��Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj��Dk	�Dk��Dl	�Dl��Dm	�Dm��Dn	�Dn��Do	�Do��Dp	�Dp��Dq	�Dq��Dr	�Dr��Ds	�Ds��Dt	�Dt�4Dy� D��D�>gD�k3D��gD�gD�NgD���D�њD� D�H D�~gD���D��D�A�DڎgD��3D��D�;3D�t�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�+A�-A�/A�-A�JA��A��;A�AͼjAͬA͡�A͙�A͑hA͏\A͉7A̓A�~�A�p�A�\)A�C�A��A��mA̺^A̝�A�ffA�9XA� �A�%A˥�A�33A�Aư!A�^5A���A�/A�&�Aã�A��
A�I�A�ĜA�E�A�A��A�JA�A�=qA���A��!A�33A��yA��7A��jA�K�A��mA�dZA�VA� �A���A��A�XA�A�A�VA��A�VA�?}A��A���A�v�A���A���A���A��uA���A��hA�  A�^5A��TA�n�A�~�A�ȴA�A�/A�`BA��A���A�{A�K�A�x�A�1'A���A���A�t�A�5?A��+A��
A���A� �A�l�A�?}A��9A�ĜA��TA���A�I�A���A��+A���A�/A�^5A�E�A�n�A�oA��jA���A�1A~�\A|�uA{�hAy+Av��Au�Aq�hAoƨAmC�Ak%AiXAgl�Ae��Ab��Aa�hA_�hA]�-A[dZAY7LAW��AUƨATA�AR�uAQO�AO%AMS�AKVAF�AE\)ABM�A?��A?33A=C�A;�A9�A8��A8A6��A5A3�hA2��A2A�A2JA1�7A1
=A0A�A.�\A+t�A*��A*n�A)�PA(ffA'��A&�+A$�HA$ �A"�yA"�RA!��A!�A ȴAK�AA�A/A�A�RA�A5?Ap�A�DA��AdZA
=AVAVA��A�A�A��A�TAC�A�^A�wA��A�/A�yA%A
�A	��A�FA�#A�RA��A�wAl�A��@�33@�bN@��^@���@�
=@�J@��@�G�@�A�@�1@��;@�
=@�@�ȴ@�^@�7@��`@��@��@�7@�7L@�@��/@�D@�\@�o@�^@�G�@�`B@�x�@�`B@�9@�dZ@�1@�5?@���@�  @�"�@�/@��m@�|�@�-@�7L@Ͼw@��@Ͳ-@��;@�ȴ@�=q@�x�@��@�1@�S�@�33@�=q@�/@���@�Ĝ@���@�S�@�o@�@�@��@��@���@�o@�o@�ȴ@\@+@°!@�n�@��-@�G�@���@���@�@��h@�X@�z�@���@���@�t�@�@���@���@��@��!@���@��H@�+@��@�K�@��^@�`B@���@�j@�9X@�Q�@�9X@�|�@�v�@�E�@���@�$�@�
=@�dZ@�"�@���@�^5@���@��#@�`B@�Ĝ@���@�Ĝ@�|�@�\)@��R@��\@�~�@�E�@���@��^@��7@���@��T@��^@��7@�p�@�V@���@�I�@���@�ƨ@��@���@�^5@���@���@�x�@�`B@�?}@��u@�I�@�1'@�b@��P@��@�  @���@�o@�ff@��#@���@�p�@�&�@���@���@�j@�"�@�@��@��y@���@�n�@�E�@���@�Ĝ@��j@��j@��@��9@�Z@��j@���@��;@�l�@���@���@�z�@�r�@��@��@���@��@���@��^@�x�@�hs@�7L@�Ĝ@�9X@�t�@�K�@�o@�o@��R@�-@��@��^@��@�X@�/@�7L@�V@�Ĝ@��@�bN@��;@�t�@�C�@��y@�ȴ@�~�@�{@��-@��-@���@���@�@�x�@�G�@�7L@�7L@�7L@�/@��@��j@�Q�@�Q�@�1'@�ƨ@�
=@���@�~�@��@���@��^@�X@���@�r�@��m@�t�@�t�@��@�|�@�K�@�
=@��H@��R@���@�^5@�J@���@��-@��h@�p�@�hs@�X@�&�@�%@���@��j@��u@�A�@�b@��@�dZ@�K�@�33@��@�
=@�
=@�^5@�ƨ@�n�@�M�@z��@p�u@hĜ@`��@V��@O��@Kt�@EV@?|�@:�H@6$�@0  @)�^@%��@ r�@��@��@$�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�+A�-A�/A�-A�JA��A��;A�AͼjAͬA͡�A͙�A͑hA͏\A͉7A̓A�~�A�p�A�\)A�C�A��A��mA̺^A̝�A�ffA�9XA� �A�%A˥�A�33A�Aư!A�^5A���A�/A�&�Aã�A��
A�I�A�ĜA�E�A�A��A�JA�A�=qA���A��!A�33A��yA��7A��jA�K�A��mA�dZA�VA� �A���A��A�XA�A�A�VA��A�VA�?}A��A���A�v�A���A���A���A��uA���A��hA�  A�^5A��TA�n�A�~�A�ȴA�A�/A�`BA��A���A�{A�K�A�x�A�1'A���A���A�t�A�5?A��+A��
A���A� �A�l�A�?}A��9A�ĜA��TA���A�I�A���A��+A���A�/A�^5A�E�A�n�A�oA��jA���A�1A~�\A|�uA{�hAy+Av��Au�Aq�hAoƨAmC�Ak%AiXAgl�Ae��Ab��Aa�hA_�hA]�-A[dZAY7LAW��AUƨATA�AR�uAQO�AO%AMS�AKVAF�AE\)ABM�A?��A?33A=C�A;�A9�A8��A8A6��A5A3�hA2��A2A�A2JA1�7A1
=A0A�A.�\A+t�A*��A*n�A)�PA(ffA'��A&�+A$�HA$ �A"�yA"�RA!��A!�A ȴAK�AA�A/A�A�RA�A5?Ap�A�DA��AdZA
=AVAVA��A�A�A��A�TAC�A�^A�wA��A�/A�yA%A
�A	��A�FA�#A�RA��A�wAl�A��@�33@�bN@��^@���@�
=@�J@��@�G�@�A�@�1@��;@�
=@�@�ȴ@�^@�7@��`@��@��@�7@�7L@�@��/@�D@�\@�o@�^@�G�@�`B@�x�@�`B@�9@�dZ@�1@�5?@���@�  @�"�@�/@��m@�|�@�-@�7L@Ͼw@��@Ͳ-@��;@�ȴ@�=q@�x�@��@�1@�S�@�33@�=q@�/@���@�Ĝ@���@�S�@�o@�@�@��@��@���@�o@�o@�ȴ@\@+@°!@�n�@��-@�G�@���@���@�@��h@�X@�z�@���@���@�t�@�@���@���@��@��!@���@��H@�+@��@�K�@��^@�`B@���@�j@�9X@�Q�@�9X@�|�@�v�@�E�@���@�$�@�
=@�dZ@�"�@���@�^5@���@��#@�`B@�Ĝ@���@�Ĝ@�|�@�\)@��R@��\@�~�@�E�@���@��^@��7@���@��T@��^@��7@�p�@�V@���@�I�@���@�ƨ@��@���@�^5@���@���@�x�@�`B@�?}@��u@�I�@�1'@�b@��P@��@�  @���@�o@�ff@��#@���@�p�@�&�@���@���@�j@�"�@�@��@��y@���@�n�@�E�@���@�Ĝ@��j@��j@��@��9@�Z@��j@���@��;@�l�@���@���@�z�@�r�@��@��@���@��@���@��^@�x�@�hs@�7L@�Ĝ@�9X@�t�@�K�@�o@�o@��R@�-@��@��^@��@�X@�/@�7L@�V@�Ĝ@��@�bN@��;@�t�@�C�@��y@�ȴ@�~�@�{@��-@��-@���@���@�@�x�@�G�@�7L@�7L@�7L@�/@��@��j@�Q�@�Q�@�1'@�ƨ@�
=@���@�~�@��@���@��^@�X@���@�r�@��m@�t�@�t�@��@�|�@�K�@�
=@��H@��R@���@�^5@�J@���@��-@��h@�p�@�hs@�X@�&�@�%@���@��j@��u@�A�@�b@��@�dZ@�K�@�33@��@�
=@�
=G�O�@�ƨ@�n�@�M�@z��@p�u@hĜ@`��@V��@O��@Kt�@EV@?|�@:�H@6$�@0  @)�^@%��@ r�@��@��@$�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
R�B
`BB
�B
�B
�
BVB1'BL�BjBq�Bx�Bx�BjBXB&�B{B
��B
�B\B>wB=qB%�BoB$�BF�BZB^5B�B��B�B��B�;B�yB�`B�ZB�B�HBBD�B��B�}B�wB�qBĜBŢBŢBȴBĜB�B�DBu�Bo�Bu�Bx�Bu�Bk�BcTB^5BR�BP�BT�B\)B33B �B�B�B�TB�/B��B�HB�B��B�BĜB��BĜB�'B�B��B�1Bn�BS�BD�B/B#�BVB
�sB
��B
ŢB
�dB
��B
�VB
�B
v�B
ffB
[#B
T�B
M�B
?}B
(�B
�B
VB
B	�B	�)B	��B	�9B	��B	��B	�%B	y�B	l�B	`BB	P�B	E�B	9XB	-B	�B	oB	JB	B��B�B�B�BB�B��B�^B�3B��B��B�B�-B�B��B��B�B�B��B��B��B��B��B��B��B��B�{B�VB�PB�JB�=B�7B�+B�B�B~�B}�B~�B|�B{�Bx�Bs�Bo�Bn�Bl�Bk�Bk�BjBhsBhsBk�Bl�Bl�BjB`BB`BBe`BffBgmBe`BcTBu�B�B{�Bs�BhsB_;Bk�Bn�BiyBgmBbNB_;B_;B]/BW
BJ�BD�BB�BD�BD�BC�BC�BB�BG�BH�BG�BF�BE�BF�BH�BI�BJ�BL�BW
BiyBp�Bu�B{�Bs�Bp�Bv�Bu�Bx�Bz�B{�B~�B�B�B}�Bx�Bv�Bu�Br�Bn�Bn�Bn�Bt�Bv�Bz�B�B�B�1B�=B�PB�VB�PB�VB�\B�\B�hB�uB�{B��B��B��B��B��B��B��B��B��B�?B�XB�dB�qB��BĜBȴB��B��B�#B�`B�B�B��B��B��B��B��B�B�B�B�B�B�B��B��B	  B	B	B	%B		7B	
=B	DB	VB	VB	PB	\B	�B	�B	"�B	-B	1'B	49B	7LB	8RB	9XB	;dB	A�B	C�B	E�B	G�B	G�B	K�B	M�B	N�B	O�B	S�B	T�B	VB	VB	\)B	^5B	aHB	bNB	cTB	e`B	gmB	hsB	jB	l�B	l�B	m�B	m�B	n�B	q�B	r�B	u�B	z�B	~�B	�B	�B	�+B	�+B	�JB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�'B	�9B	�RB	�XB	�}B	��B	�}B	��B	��B	��B	B	B	��B	ÖB	ŢB	ŢB	ĜB	ƨB	ǮB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�)B	�)B	�)B	�)B	�)B	�)B	�)B	�5B	�;B	�;B	�;B	�;B	�BB	�HB	�HB	�HB	�NB	�HB	�HB	�NB	�HB	�HB	�NB	�TB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
  B
	7B
hB
�B
!�B
&�B
.B
33B
:^B
?}B
F�B
J�B
K�B
N�B
S�B
XB
]/B
aHB
dZB
hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
R�B
`:B
�B
��B
��BLB1BL�BjtBq�Bx�Bx�BjtBXB&�BnB
��B
�BPB>kB=gB%�BfB$�BF�BZB^'B�B�~B�B��B�/B�lB�RB�NB�B�;BBD�B��B�rB�nB�eBđBŖBŔBȩBđB�	B�4Bu�Bo�Bu�Bx�Bu�BkyBcGB^%BR�BP�BT�B\B3$B �BxB�B�GB�$B��B�:B�B��B�BďB˹BđB�B��B��B�$Bn�BS�BD�B/B#�BJB
�hB
��B
ŕB
�WB
��B
�IB
�	B
v�B
f\B
[B
T�B
M�B
?uB
(�B
�B
OB
B	�B	�$B	��B	�4B	��B	��B	� B	y�B	l�B	`>B	P�B	E�B	9UB	-B	�B	oB	KB	B��B�B�B�DB�	B��B�`B�8B��B��B�B�/B�B��B��B�B�B�B��B��B��B��B��B��B��B��B�[B�TB�MB�@B�;B�0B�B�B~�B}�B B|�B{�Bx�Bs�Bo�Bn�Bl�Bk�Bk�Bj�BhxBh{Bk�Bl�Bl�Bj�B`HB`IBeeBfmBgrBefBcXBu�B�#B{�Bs�BhwB_ABk�Bn�Bi}BgqBbUB_@B_AB]4BWBJ�BD�BB�BD�BD�BC�BC�BB�BG�BH�BG�BF�BE�BF�BH�BI�BJ�BL�BWBi}Bp�Bu�B{�Bs�Bp�Bv�Bu�Bx�Bz�B{�B~�B�B�"B}�Bx�Bv�Bu�Br�Bn�Bn�Bn�Bt�Bv�Bz�B�B�"B�3B�=B�SB�VB�SB�XB�]B�]B�iB�tB�{B��B��B��B��B��B��B��B��B��B�>B�TB�eB�pB��BěBȳB��B��B� B�[B�B�B��B��B��B��B��B�B�B�B�B�B�B��B��B��B	B		B	B		2B	
7B	@B	RB	SB	KB	YB	B	�B	"�B	-B	1"B	43B	7FB	8KB	9QB	;\B	A�B	C�B	E�B	G�B	G�B	K�B	M�B	N�B	O�B	S�B	T�B	U�B	U�B	\B	^.B	a>B	bDB	cNB	eWB	geB	hlB	juB	l�B	l�B	m�B	m�B	n�B	q�B	r�B	u�B	z�B	~�B	�B	�B	�!B	�!B	�AB	�LB	�fB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�"B	�B	�-B	�FB	�NB	�sB	�yB	�pB	�{B	�zB	�}B	B	B	��B	ÊB	ŗB	ŖB	đB	ƝB	ǢB	ǥB	ȩB	ɰB	ɱB	˹B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�,B	�/B	�1B	�-B	�0B	�5B	�:B	�=B	�<B	�BB	�<B	�<B	�AB	�;B	�;B	�>B	�FB	�NB	�QB	�UB	�XB	�YB	�YB	�ZB	�bB	�aB	�hB	�mB	�lB	�lB	�lB	�mB	�qB	�rB	�pB	�pB	�qB	�yB	�yB	�B	�B	�B	�B	�B	�B	�G�O�B	�B	��B
	)B
\B
�B
!�B
&�B
.B
3(B
:OB
?mB
F�B
J�B
K�B
N�B
S�B
XB
]B
a9B
dJB
hc11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.15 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436492016080714364920160807143649  AO  ARCAADJP                                                                    20160724050145    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160724050145  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160724050145  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143649  IP                  G�O�G�O�G�O�                