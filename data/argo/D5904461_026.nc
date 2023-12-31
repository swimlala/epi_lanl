CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:48Z AOML 3.0 creation; 2016-08-07T21:36:31Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221348  20160807143631  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_026                   2C  D   APEX                            6531                            072314                          846 @�3K��	1   @�3L��?�@29XbN�d�;dZ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Cs�3CvL�Cw�3Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� DdfDd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDys3D� D�0 D�� D�ٚD�3D�9�D���D�� D�  D�0 D��3D��3D�3D�@ D�|�D���D���D�<�D�l�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@���AffA"ffABffAbffA�33A�33A�33A�33A�33A�33A�33A�33B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B�� B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�� B�L�B�L�B�L�C &fC&fC&fC&fC&fC
&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC &fC"&fC$&fC&&fC(&fC*&fC,&fC.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd&fCf&fCh&fCj&fCl&fCn&fCp&fCr@ CsٙCvs3CwٙCz&fC|&fC~&fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D ��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D		�D	��D
	�D
��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D 	�D ��D!	�D!��D"	�D"��D#	�D#��D$	�D$��D%	�D%��D&	�D&��D'	�D'��D(	�D(��D)	�D)��D*	�D*��D+	�D+��D,	�D,��D-	�D-��D.	�D.��D/	�D/��D0	�D0��D1	�D1��D2	�D2�4D3	�D3��D4	�D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8��D9	�D9��D:	�D:��D;	�D;��D<	�D<��D=	�D=��D>	�D>��D?	�D?��D@	�D@��DA	�DA��DB	�DB��DC	�DC��DD	�DD��DE	�DE��DF	�DF��DG	�DG��DH	�DH��DI	�DI��DJ	�DJ��DK	�DK��DL	�DL��DM	�DM��DN	�DN��DO	�DO��DP	�DP��DQ	�DQ��DR	�DR��DS	�DS��DT	�DT��DU	�DU��DV	�DV��DW	�DW��DX	�DX��DY	�DY��DZ	�DZ��D[	�D[��D\	�D\��D]	�D]��D^	�D^��D_	�D_��D`	�D`��Da	�Da��Db	�Db��Dc	�Dc��Dd Dd��De	�De��Df	�Df��Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj��Dk	�Dk��Dl	�Dl��Dm	�Dm��Dn	�Dn��Do	�Do��Dp	�Dp��Dq	�Dq��Dr	�Dr��Ds	�Ds��Dt	�Dtp Dy|�D��D�4�D���D��gD� D�>gD���D���D��D�4�D�� D�� D� D�D�Dځ�D�њD��D�A�D�q�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A���A�M�A�;dA�7LA�E�A�dZA͏\A͙�A͏\A͉7A�~�Aͣ�A�"�A��A�ƨA͝�AͶFAͬA͇+A�jA�t�A�jA�ZA�O�A�K�A�Q�A�M�A�E�A�=qA��A�JA���A��mẢ7A�bAˣ�A��yA�~�A�ZA�JA�z�A�=qA®A��A�Q�A�=qA�33A���A���A���A���A�1'A�~�A���A��\A�"�A��\A�  A��/A�A��FA�S�A��jA��DA�K�A��HA��wA���A�jA�{A��TA���A�1'A�z�A��9A�l�A�9XA��uA���A���A���A��hA���A�bA�O�A�
=A��;A�v�A�n�A��A}��Ay&�Av�As��Aqx�AnM�AmG�AlI�Aj�\Aj �Aix�AdA�Aa��A^��AZ�RAV1AQ�TAP��AO&�AK��AEƨAC��ABbNAA�7A?�PA;�A9�A7A5O�A5"�A4(�A3�A3�^A3+A1O�A/�-A.jA,��A+�A*r�A)t�A(��A'��A&��A%VA$�A#;dA"�9A"9XA"  A!S�A �/A ��A A�A/A��AffA�mA��A�;A;dAbNA�mA�AAffA�AQ�A�A �A|�A��Ap�A~�At�A��A��A
��A
 �A�9A�^A��A�\A1'A�AJAt�A��AȴAbNA/AƨA �HA V@��@���@�p�@��`@��@�33@��T@�`B@��`@�1'@���@��+@��#@��@�D@�ƨ@�dZ@�C�@�v�@�@�O�@�/@��@�1'@�  @�ƨ@�"�@�=q@�@�bN@�"�@��T@�G�@�X@陚@�@�9@��@�;d@�^@��@�~�@��@ݩ�@݉7@�`B@��@���@ٲ-@ج@��@�j@�@��`@�(�@�C�@ҟ�@҇+@�V@�5?@љ�@��@���@�7L@�Ĝ@̣�@�9X@��@���@�ƨ@˕�@�S�@ʰ!@�$�@�7L@��`@ȋD@�9X@�1'@��m@Ǯ@�t�@�S�@�C�@�+@�o@�o@��H@Ƨ�@�^5@���@�`B@�&�@���@���@��@�r�@�(�@�9X@�  @ÍP@�33@��@�@�ff@�V@��T@�7L@��D@�r�@�  @���@�K�@��!@��T@���@�@���@��/@�(�@���@�A�@�A�@�A�@�9X@���@��@�bN@���@�bN@��w@�"�@�ff@�@��@��j@�9X@��w@�"�@��H@��\@�-@�@��#@��h@�p�@�`B@��u@��m@��@�l�@�C�@�
=@���@�$�@��#@�hs@��@�9X@��@�S�@�+@���@���@��!@�~�@�V@��@��@�x�@��`@���@��9@�1'@��m@���@��F@�K�@���@���@�M�@�hs@�bN@�1'@��@��;@��w@��@�|�@��y@�J@�x�@���@��j@��D@��D@��D@�j@��D@��j@��u@�Q�@��@�b@��@��w@���@��@�|�@�dZ@�;d@�+@���@�n�@�-@��-@�X@�?}@��@�z�@�Q�@�  @��w@�|�@���@��@�ȴ@�5?@���@���@��@��`@��`@��9@�Q�@��@��w@���@�+@��@���@�n�@�$�@�G�@��u@�Q�@�Q�@�1@�ƨ@�A�@���@�r�@��w@��@�=q@�ff@��@�X@�&�@��9@�  @��F@�dZ@�+@��@�n�@�@���@�X@��`@���@���@�j@��@��
@��F@���@�dZ@�C�@�
=@���@��@���@�ȴ@���@���@�~�@�^5@�J@��T@�@���@�`B@��@���@���@��D@�1'@��;@��@�@���@�v�@�z�@�V@z�H@r-@iX@`Ĝ@XĜ@Q�#@J=q@B��@<(�@5p�@0�9@*n�@$��@ b@o@�T@�@j@Ĝ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��A���A�M�A�;dA�7LA�E�A�dZA͏\A͙�A͏\A͉7A�~�Aͣ�A�"�A��A�ƨA͝�AͶFAͬA͇+A�jA�t�A�jA�ZA�O�A�K�A�Q�A�M�A�E�A�=qA��A�JA���A��mẢ7A�bAˣ�A��yA�~�A�ZA�JA�z�A�=qA®A��A�Q�A�=qA�33A���A���A���A���A�1'A�~�A���A��\A�"�A��\A�  A��/A�A��FA�S�A��jA��DA�K�A��HA��wA���A�jA�{A��TA���A�1'A�z�A��9A�l�A�9XA��uA���A���A���A��hA���A�bA�O�A�
=A��;A�v�A�n�A��A}��Ay&�Av�As��Aqx�AnM�AmG�AlI�Aj�\Aj �Aix�AdA�Aa��A^��AZ�RAV1AQ�TAP��AO&�AK��AEƨAC��ABbNAA�7A?�PA;�A9�A7A5O�A5"�A4(�A3�A3�^A3+A1O�A/�-A.jA,��A+�A*r�A)t�A(��A'��A&��A%VA$�A#;dA"�9A"9XA"  A!S�A �/A ��A A�A/A��AffA�mA��A�;A;dAbNA�mA�AAffA�AQ�A�A �A|�A��Ap�A~�At�A��A��A
��A
 �A�9A�^A��A�\A1'A�AJAt�A��AȴAbNA/AƨA �HA V@��@���@�p�@��`@��@�33@��T@�`B@��`@�1'@���@��+@��#@��@�D@�ƨ@�dZ@�C�@�v�@�@�O�@�/@��@�1'@�  @�ƨ@�"�@�=q@�@�bN@�"�@��T@�G�@�X@陚@�@�9@��@�;d@�^@��@�~�@��@ݩ�@݉7@�`B@��@���@ٲ-@ج@��@�j@�@��`@�(�@�C�@ҟ�@҇+@�V@�5?@љ�@��@���@�7L@�Ĝ@̣�@�9X@��@���@�ƨ@˕�@�S�@ʰ!@�$�@�7L@��`@ȋD@�9X@�1'@��m@Ǯ@�t�@�S�@�C�@�+@�o@�o@��H@Ƨ�@�^5@���@�`B@�&�@���@���@��@�r�@�(�@�9X@�  @ÍP@�33@��@�@�ff@�V@��T@�7L@��D@�r�@�  @���@�K�@��!@��T@���@�@���@��/@�(�@���@�A�@�A�@�A�@�9X@���@��@�bN@���@�bN@��w@�"�@�ff@�@��@��j@�9X@��w@�"�@��H@��\@�-@�@��#@��h@�p�@�`B@��u@��m@��@�l�@�C�@�
=@���@�$�@��#@�hs@��@�9X@��@�S�@�+@���@���@��!@�~�@�V@��@��@�x�@��`@���@��9@�1'@��m@���@��F@�K�@���@���@�M�@�hs@�bN@�1'@��@��;@��w@��@�|�@��y@�J@�x�@���@��j@��D@��D@��D@�j@��D@��j@��u@�Q�@��@�b@��@��w@���@��@�|�@�dZ@�;d@�+@���@�n�@�-@��-@�X@�?}@��@�z�@�Q�@�  @��w@�|�@���@��@�ȴ@�5?@���@���@��@��`@��`@��9@�Q�@��@��w@���@�+@��@���@�n�@�$�@�G�@��u@�Q�@�Q�@�1@�ƨ@�A�@���@�r�@��w@��@�=q@�ff@��@�X@�&�@��9@�  @��F@�dZ@�+@��@�n�@�@���@�X@��`@���@���@�j@��@��
@��F@���@�dZ@�C�@�
=@���@��@���@�ȴ@���@���@�~�@�^5@�J@��T@�@���@�`B@��@���@���@��D@�1'@��;@��@�@���G�O�@�z�@�V@z�H@r-@iX@`Ĝ@XĜ@Q�#@J=q@B��@<(�@5p�@0�9@*n�@$��@ b@o@�T@�@j@Ĝ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	�!B	�wB	��B	��B	��B	��B	�
B	�B
?}B
�oB
��B
�B
B
ȴB
ƨB
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�HB
�`B
�mB
�B
��BBuB,B>wBB�B[#B{�B�\B��B�3B�^B��B�XB��B��B��B��B��B��B��B��B�3B�LB�dB�!B��B�Bz�B]/BF�B'�B{B	7BB
�sB
��B
��B
B
B
�TB
ɺB
�dB
ƨB
��B
�9B
��B
�JB
�+B
x�B
YB
A�B
33B
�B
B	�B	ȴB	�B	��B	�7B	|�B	o�B	p�B	k�B	]/B	XB	P�B	33B	!�B	hB��B�yB�)B�B��BĜB�qB�XB�RB�?B�-B�B��B�B�'B�-B�'B�'B�3B�FB�XB�^B�^B�dB�^B�dB�qB�wB�qB�^B�FB�?B�?B�LB�XB�^B�jB��BBÖBɺBǮBǮB��B��B��B��B��B�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��BȴBŢBĜBɺB��B��B��B��B��B�B�5B�ZB�ZB�TB�NB�HB�;B�;B�;B�5B�5B�5B�/B�)B�)B�B�B�B�#B�)B�/B�/B�/B�;B�NB�NB�TB�`B�mB�mB�mB�sB�yB�B�B�B�B�B�B��B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	+B	%B	B��B��B	B	B	1B	JB	PB	\B	uB	hB	hB	{B	�B	�B	!�B	"�B	#�B	$�B	$�B	(�B	/B	49B	6FB	7LB	:^B	?}B	A�B	E�B	I�B	L�B	M�B	N�B	P�B	R�B	W
B	\)B	_;B	aHB	ffB	jB	l�B	n�B	q�B	s�B	w�B	x�B	y�B	z�B	}�B	�B	�B	�B	�B	�1B	�=B	�=B	�7B	�7B	�=B	�=B	�JB	�\B	�bB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�FB	�jB	�}B	��B	B	B	B	B	B	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�)B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�/B	�)B	�B	�B	�
B	�
B	�B	�#B	�/B	�;B	�BB	�;B	�BB	�BB	�HB	�TB	�ZB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
  B
  B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
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
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
	7B

=B
JB
�B
�B
#�B
(�B
.B
49B
9XB
@�B
D�B
M�B
Q�B
T�B
[#B
_;B
dZB
hsB
m�B
q�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	��B	��B	��B	��B	��B	�B	�xB	��B	��B	��B	��B	�B	�B
?wB
�gB
��B
��B
B
ȪB
ƞB
˿B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�@B
�WB
�dB
�B
��BBnB+�B>lBB�B[B{�B�OB��B�&B�OB�vB�MBʳB��B��B��B��B��B��B��B�&B�?B�YB�B�sB�Bz�B]#BF�B'�BlB	,B �B
�fB
ʸB
��B
B
B
�FB
ɫB
�XB
ƚB
�zB
�0B
��B
�BB
�!B
x�B
YB
A�B
3,B
�B
 �B	�wB	ȫB	��B	��B	�2B	|�B	o�B	p�B	k�B	].B	XB	P�B	32B	!�B	gB��B�zB�,B�B��BğB�tB�ZB�VB�CB�0B� B��B�B�+B�0B�(B�(B�4B�JB�ZB�aB�bB�gB�`B�iB�tB�xB�sB�bB�IB�BB�AB�PB�XB�bB�lB��BB×BɼBǯBǭB��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��BȵBŢBĝBɸB��B��B��B��B��B�B�4B�ZB�YB�RB�LB�FB�:B�;B�:B�4B�4B�4B�+B�(B�)B�B�B�B�"B�(B�+B�.B�/B�9B�KB�NB�SB�]B�jB�kB�jB�qB�xB�B�B�|B�B�B�B��B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	'B	!B	B��B��B	B	B	-B	FB	LB	YB	qB	dB	cB	xB	�B	�B	!�B	"�B	#�B	$�B	$�B	(�B	/B	44B	6?B	7FB	:XB	?wB	A�B	E�B	I�B	L�B	M�B	N�B	P�B	R�B	WB	\!B	_3B	aCB	f]B	jyB	l�B	n�B	q�B	s�B	w�B	x�B	y�B	z�B	}�B	��B	�B	�B	�B	�'B	�6B	�6B	�/B	�,B	�6B	�5B	�@B	�SB	�XB	�eB	�cB	�rB	��B	��B	��B	��B	��B	��B	��B	�
B	�:B	�aB	�rB	�B	B	B	B	B	B	ÉB	đB	ŗB	ŖB	ƞB	ǤB	ǥB	ȩB	ɰB	ʵB	ɮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	�	B	�
B	�B	�B	�B	�B	�B	�B	�%B	�"B	�"B	�$B	�#B	�$B	�B	�B	�B	��B	��B	�	B	�B	�#B	�,B	�4B	�-B	�6B	�8B	�;B	�GB	�LB	�SB	�aB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
�B	��B	��B
B
 B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 B
B
�B
�B
�B
�B
B
B
B
B

B
B
B
B
B
B
B
B
!B
$B
	+G�O�B
<B
tB
�B
#�B
(�B
.B
4+B
9LB
@tB
D�B
M�B
Q�B
T�B
[B
_.B
dGB
hbB
m�B
q�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.15 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436312016080714363120160807143631  AO  ARCAADJP                                                                    20150226221348    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221348  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221348  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143631  IP                  G�O�G�O�G�O�                