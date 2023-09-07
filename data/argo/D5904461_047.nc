CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-05-08T02:15:59Z AOML 3.0 creation; 2016-08-07T21:36:35Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150508021559  20160807143635  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               /A   AO  5286_8897_047                   2C  D   APEX                            6531                            072314                          846 @�N�_1�
1   @�N���?�@2�\(��c�p��
=1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    /A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C/�fC2  C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO�fDP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy� D��D�9�D���D��3D�	�D�9�D��fD���D�3D�C3D���D��3D�3D�ffD�y�D� D�3D�33D�3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���AffA"ffABffAbffA�33A�33A�33A�33A�33A�33A�33A�33B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C &fC&fC�C&fC&fC
&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC &fC"&fC$&fC&&fC(&fC*&fC,&fC.@ C0�C2&fC4�C6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd&fCf&fCh&fCj@ Cl&fCn&fCp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�fC�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D ��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D		�D	��D
	�D
��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D 	�D ��D!	�D!��D"	�D"��D#	�D#��D$	�D$��D%	�D%��D&	�D&��D'	�D'��D(	�D(��D)	�D)��D*	�D*��D+	�D+��D,	�D,��D-	�D-��D.	�D.��D/	�D/��D0	�D0��D1	�D1��D2	�D2��D3	�D3��D4	�D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8��D9	�D9��D:	�D:��D;	�D;��D<	�D<��D=	�D=��D>	�D>��D?	�D?��D@	�D@��DA	�DA��DB	�DB��DC	�DC��DD	�DD��DE	�DE��DF	�DF��DG	�DG��DH	�DH��DI	�DI��DJ	�DJ��DK	�DK��DL	�DL��DM	�DM��DN	�DN��DO	�DO� DP	�DP��DQ	�DQ��DR	�DR��DS	�DS��DT	�DT��DU	�DU��DV	�DV��DW	�DW��DX	�DX��DY	�DY��DZ	�DZ��D[	�D[��D\	�D\��D]	�D]��D^	�D^��D_	�D_��D`	�D`��Da	�Da��Db	�Db��Dc	�Dc��Dd	�Dd��De	�De��Df	�Df��Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj��Dk	�Dk��Dl	�Dl��Dm	�Dm��Dn	�Dn��Do	�Do��Dp	�Dp��Dq	�Dq��Dr	�Dr��Ds	�Ds��Dt	�Dt�4Dy��D�!�D�>gD��gD�� D�gD�>gD��3D�њD� D�H D��gD�� D� D�k3D�~gD��D� D�8 D� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�r�A�^5A�G�A�(�A�{A�1A���A��A��A��`A��`A��HA��TA��TA��HA��HA��HA��HA��HA��;A��
A��
A���A���A���A���A���A���A���A�ȴA�ȴA�A̼jA̶FA̮Ạ�A̙�A̍PA�z�A�p�A�jA�dZA�bNA�bNA�bNA�ZA�?}A�"�A���A�n�A�M�A�C�A�9XA�/A�+A�&�A��A�VA�1A���A��mA�A�r�A�+A��`A�n�A�1'A���A���A�"�A���A�n�A�1'A���A�M�A�-A�1'A���A���A���A®A�AA�dZA��;A�VA���A�hsA���A�A��/A���A���A���A�A�A� �A�t�A�|�A�JA�z�A��A�I�A���A��A���A��9A���A�M�A�ffA���A���A�$�A���A��A��FA�M�A�~�A���A�VA�r�A�(�A��mA�VA��;A���A�"�A�M�A��mA}oA{VAyoAxVAv��AvM�Au33Ar1ApE�Al�`Ah�A`�`A\{AY33AX{AWdZAUC�ARz�AQ�-APJAM��AI�AG�
ABz�A@��A>�RA<A�A:�uA9A8��A8n�A8A7��A7�A7&�A6�A65?A5x�A4�+A3K�A2�A0I�A/��A//A.��A.Q�A-�wA,�!A+VA)+A(1A'�A&��A&��A&�DA&��A&��A&�A&1A"jAS�A&�A�DAt�A5?A;dA
=A��A��AE�A��A��A�At�A��A(�A{AbA��A��Ax�Al�A�A~�AbA��A��A`BA�/A5?A��A
$�A	G�A-Al�A7LA~�A�A�mA@���@�=q@�?}@���@��#@�
=@�bN@�h@웦@��m@�"�@�hs@�{@�F@㝲@��@�P@�n�@�^@���@߾w@�"�@�Q�@�+@ڏ\@�$�@�/@�1@�Q�@؋D@؋D@�-@ڇ+@���@�\)@���@�G�@���@�1@�C�@�M�@�j@�~�@��T@ͩ�@�%@̴9@̓u@�z�@�(�@˶F@�A�@��@�  @�ƨ@��@�ff@�ff@���@ȼj@�ff@�ƨ@�V@�$�@���@��@���@�bN@�  @�(�@��m@� �@��@�j@�Q�@�j@��D@���@�
=@��@���@�@���@�t�@��y@�^5@�$�@��@���@���@�hs@�?}@���@��/@�z�@�1@�;d@���@�=q@��T@�x�@��@���@��u@��D@�Z@��
@��w@���@��@�dZ@�"�@���@���@�ff@��@��T@���@���@��h@�x�@��`@�Z@� �@���@�o@�o@�o@��y@��R@��!@��+@�{@�?}@��@���@���@�9X@��P@�C�@�+@���@�~�@�{@��@��^@��h@�G�@�Ĝ@�b@�l�@�K�@�S�@�S�@�K�@���@���@��^@��@�p�@�`B@�X@�&�@�V@��/@��D@�Z@�1@���@���@��P@�S�@���@���@���@��H@���@�$�@��-@�hs@�O�@���@��`@�1'@��F@��H@��R@��R@���@��-@���@��u@�j@�Q�@�9X@�(�@��@�ƨ@��@�|�@�S�@�@�v�@�@��h@��h@��@�%@��9@�j@�(�@��@���@��w@���@�|�@�S�@�"�@��@�@��R@�-@��T@��h@�p�@�&�@��@�z�@�b@�ƨ@�|�@�K�@��H@�E�@���@�?}@�Ĝ@�bN@� �@��
@�|�@��@��H@�~�@�=q@�@�p�@�hs@�`B@�`B@�O�@�7L@��@��@�Ĝ@��9@�z�@�bN@�I�@�  @��-@���@z~�@sdZ@j-@`r�@Wl�@P1'@IG�@Co@;C�@5�T@1G�@+ƨ@%O�@ �`@5?@M�@5?@�^@`B11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�r�A�^5A�G�A�(�A�{A�1A���A��A��A��`A��`A��HA��TA��TA��HA��HA��HA��HA��HA��;A��
A��
A���A���A���A���A���A���A���A�ȴA�ȴA�A̼jA̶FA̮Ạ�A̙�A̍PA�z�A�p�A�jA�dZA�bNA�bNA�bNA�ZA�?}A�"�A���A�n�A�M�A�C�A�9XA�/A�+A�&�A��A�VA�1A���A��mA�A�r�A�+A��`A�n�A�1'A���A���A�"�A���A�n�A�1'A���A�M�A�-A�1'A���A���A���A®A�AA�dZA��;A�VA���A�hsA���A�A��/A���A���A���A�A�A� �A�t�A�|�A�JA�z�A��A�I�A���A��A���A��9A���A�M�A�ffA���A���A�$�A���A��A��FA�M�A�~�A���A�VA�r�A�(�A��mA�VA��;A���A�"�A�M�A��mA}oA{VAyoAxVAv��AvM�Au33Ar1ApE�Al�`Ah�A`�`A\{AY33AX{AWdZAUC�ARz�AQ�-APJAM��AI�AG�
ABz�A@��A>�RA<A�A:�uA9A8��A8n�A8A7��A7�A7&�A6�A65?A5x�A4�+A3K�A2�A0I�A/��A//A.��A.Q�A-�wA,�!A+VA)+A(1A'�A&��A&��A&�DA&��A&��A&�A&1A"jAS�A&�A�DAt�A5?A;dA
=A��A��AE�A��A��A�At�A��A(�A{AbA��A��Ax�Al�A�A~�AbA��A��A`BA�/A5?A��A
$�A	G�A-Al�A7LA~�A�A�mA@���@�=q@�?}@���@��#@�
=@�bN@�h@웦@��m@�"�@�hs@�{@�F@㝲@��@�P@�n�@�^@���@߾w@�"�@�Q�@�+@ڏ\@�$�@�/@�1@�Q�@؋D@؋D@�-@ڇ+@���@�\)@���@�G�@���@�1@�C�@�M�@�j@�~�@��T@ͩ�@�%@̴9@̓u@�z�@�(�@˶F@�A�@��@�  @�ƨ@��@�ff@�ff@���@ȼj@�ff@�ƨ@�V@�$�@���@��@���@�bN@�  @�(�@��m@� �@��@�j@�Q�@�j@��D@���@�
=@��@���@�@���@�t�@��y@�^5@�$�@��@���@���@�hs@�?}@���@��/@�z�@�1@�;d@���@�=q@��T@�x�@��@���@��u@��D@�Z@��
@��w@���@��@�dZ@�"�@���@���@�ff@��@��T@���@���@��h@�x�@��`@�Z@� �@���@�o@�o@�o@��y@��R@��!@��+@�{@�?}@��@���@���@�9X@��P@�C�@�+@���@�~�@�{@��@��^@��h@�G�@�Ĝ@�b@�l�@�K�@�S�@�S�@�K�@���@���@��^@��@�p�@�`B@�X@�&�@�V@��/@��D@�Z@�1@���@���@��P@�S�@���@���@���@��H@���@�$�@��-@�hs@�O�@���@��`@�1'@��F@��H@��R@��R@���@��-@���@��u@�j@�Q�@�9X@�(�@��@�ƨ@��@�|�@�S�@�@�v�@�@��h@��h@��@�%@��9@�j@�(�@��@���@��w@���@�|�@�S�@�"�@��@�@��R@�-@��T@��h@�p�@�&�@��@�z�@�b@�ƨ@�|�@�K�@��H@�E�@���@�?}@�Ĝ@�bN@� �@��
@�|�@��@��H@�~�@�=q@�@�p�@�hs@�`B@�`B@�O�@�7L@��@��@�Ĝ@��9@�z�@�bN@�I�G�O�@��-@���@z~�@sdZ@j-@`r�@Wl�@P1'@IG�@Co@;C�@5�T@1G�@+ƨ@%O�@ �`@5?@M�@5?@�^@`B11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�
B	�B	�B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�
B	�B	�B	�5B	�BB	�`B	�fB	�sB	�sB	�B	��B	��B
	7B
:^B
J�B
O�B
W
B
^5B
aHB
e`B
jB
s�B
v�B
}�B
�+B
��B
�qB
�5B
��B/BL�Bv�B��B��BBJBoB�B-BJ�B_;BcTBcTBgmBiyBk�Bl�Bl�Bo�B}�B�\B�Bv�B`BBYBZBW
BR�BM�BA�B5?B/B.B,B'�B%�B%�B$�B�BPB�B'�B1'B-B#�B�B�B�BuB+B��B�%Bs�BE�BhB
��B
�ZB
�wB
�%B
cTB
49B	��B	ɺB	�^B	�B	��B	��B	��B	�bB	� B	r�B	`BB	G�B	#�B	VB	B��B��B�B�`B�HB�#B��B��B��B��B��B��BǮBƨBŢBŢBŢBŢBŢBĜBĜBB��B�qB�jB�jB�dB�jB�dB�dB�^B�XB�LB�9B�3B�'B�!B�B�B�B�!B�RBȴB�B�B��B��B��B��BɺBƨBÖBĜBĜBĜBŢBƨBǮBɺB��B��B�
B�B�B�)B�5B�5B�/B�5B�5B�BB�BB�BB�;B�BB�TB�B�sB�fB�TB�NB�sB�mB�`B�mB�;B�/B�5B�)B�B�#B�TB�/B�B�B�
B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B�
B�;B�`B�B�B	B	{B	�B	oB	hB	hB	hB	oB	oB	bB	bB	\B	VB	\B	\B	\B	bB	bB	bB	oB	�B	�B	�B	�B	�B	"�B	#�B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	$�B	(�B	/B	0!B	1'B	33B	6FB	6FB	8RB	;dB	=qB	?}B	?}B	=qB	?}B	A�B	B�B	C�B	D�B	E�B	G�B	H�B	K�B	L�B	N�B	R�B	XB	]/B	_;B	cTB	e`B	ffB	iyB	l�B	l�B	n�B	r�B	u�B	x�B	{�B	|�B	|�B	|�B	|�B	|�B	}�B	~�B	� B	� B	� B	� B	�B	�1B	�=B	�PB	�bB	�bB	�bB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�9B	�?B	�LB	�XB	�jB	�jB	�dB	�^B	�^B	�^B	�^B	�XB	�XB	�XB	�^B	�dB	�jB	�qB	�}B	�}B	�}B	�}B	��B	B	ĜB	ŢB	ƨB	ǮB	��B	��B	��B	��B	�B	�
B	�
B	�
B	�#B	�)B	�)B	�#B	�#B	�#B	�#B	�)B	�)B	�)B	�/B	�5B	�5B	�5B	�5B	�;B	�BB	�HB	�NB	�NB	�HB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
+B
%B
%B
1B

=B
�B
�B
"�B
'�B
-B
49B
:^B
?}B
E�B
K�B
N�B
Q�B
W
B
]/B
`BB
bNB
ffB
jB
o�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�	B	�
B	�B	�B	�B	��B	�B	�B	�B	�3B	�@B	�`B	�dB	�rB	�pB	�B	��B	��B
	3B
:YB
J�B
O�B
WB
^/B
aBB
eZB
jzB
s�B
v�B
}�B
�#B
��B
�iB
�,B
��B/BL�Bv�B��B��BB=B_B�B-BJ�B_-BcHBcHBgaBikBkxBl�Bl�Bo�B}�B�MB�Bv�B`6BYBZBV�BR�BM�BA}B55B/B.B+�B'�B%�B%�B$�B�BEB�B'�B1B- B#�B�B�BxBcBB��B�Bs�BE�B]B
��B
�NB
�kB
�B
cJB
40B	��B	ɲB	�ZB	�	B	��B	��B	��B	�`B	�B	r�B	`@B	G�B	#�B	YB	B��B��B�B�bB�IB�'B��B��B��B��B��B��BǯBƫBŤBţBţBťBŤBĠBĠBB��B�vB�mB�lB�jB�nB�gB�gB�`B�[B�PB�;B�5B�'B�$B�B�B�B�#B�UBȷB�B�B��B��B��B��BɺBƧB×BğBĞBĜBţBƨBǮBɺB��B��B�
B�B�B�*B�4B�6B�/B�8B�3B�BB�BB�AB�:B�AB�UB�~B�qB�dB�TB�MB�oB�mB�]B�kB�9B�.B�4B�%B�B� B�TB�-B�B�B�
B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B�	B�7B�]B�B�B	B	xB	}B	kB	bB	dB	cB	jB	lB	]B	^B	ZB	QB	YB	XB	WB	]B	]B	_B	iB	�B	�B	�B	�B	�B	"�B	#�B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	$�B	(�B	/B	0B	1#B	3,B	6?B	6AB	8JB	;`B	=jB	?wB	?vB	=jB	?yB	A�B	B�B	C�B	D�B	E�B	G�B	H�B	K�B	L�B	N�B	R�B	X	B	]&B	_3B	cLB	eWB	f\B	isB	l�B	l�B	n�B	r�B	u�B	x�B	{�B	|�B	|�B	|�B	|�B	|�B	}�B	~�B	�B	�B	�B	�B	�B	�,B	�6B	�DB	�YB	�[B	�[B	�eB	�qB	�qB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�5B	�BB	�LB	�^B	�_B	�YB	�RB	�SB	�YB	�RB	�KB	�LB	�LB	�TB	�ZB	�_B	�fB	�pB	�qB	�rB	�qB	�B	B	ēB	ŗB	ƝB	ǤB	ʸB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�)B	�,B	�)B	�*B	�/B	�6B	�<B	�BB	�@B	�<B	�HB	�LB	�SB	�YB	�\B	�YB	�ZB	�ZB	�_B	�bB	�hB	�fB	�qB	�xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 B
�B
B
B
B
B
B
B
B
B
G�O�B

/B
sB
�B
"�B
'�B
- B
4,B
:OB
?nB
E�B
K�B
N�B
Q�B
V�B
]B
`0B
bAB
fVB
joB
o�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.15 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436352016080714363520160807143635  AO  ARCAADJP                                                                    20150508021559    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150508021559  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150508021559  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143635  IP                  G�O�G�O�G�O�                