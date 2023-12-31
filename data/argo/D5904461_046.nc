CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-05-02T19:17:05Z AOML 3.0 creation; 2016-08-07T21:36:34Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150502191705  20160807143635  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               .A   AO  5286_8897_046                   2C  D   APEX                            6531                            072314                          846 @�M�5H��1   @�M����@2�9XbN�c�-1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    .A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D?��D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd�fDe  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�fD�3D�0 D�p D���D�3D�C3D�� D��fD�  D�<�D�vfD�� D� D�FfDڙ�D���D��D�L�D�i�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @���AffA"ffABffAbffA�33A�33A�33A�33A�33A�33A�33A�33B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C &fC&fC&fC&fC&fC
&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC@ C &fC"&fC$�C&&fC(&fC*&fC,&fC.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd&fCf&fCh&fCj&fCl&fCn&fCp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D ��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D		�D	��D
	�D
��D	�D��D	�D��D	�D��D	�D��D4D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D 	�D ��D!	�D!��D"	�D"��D#	�D#��D$	�D$��D%	�D%��D&	�D&��D'	�D'��D(	�D(��D)	�D)��D*	�D*��D+	�D+��D,	�D,��D-	�D-��D.	�D.��D/	�D/��D0	�D0��D1	�D1��D2	�D2��D3	�D3��D4	�D4��D5	�D5��D6	�D6�4D7	�D7��D8	�D8��D9	�D9��D:	�D:��D;	�D;��D<	�D<��D=	�D=��D>	�D>��D?	�D?��D@4D@��DA	�DA��DB	�DB��DC	�DC��DD	�DD��DE	�DE��DF	�DF��DG	�DG��DH	�DH��DI	�DI��DJ	�DJ��DK	�DK��DL	�DL��DM	�DM��DN	�DN��DO	�DO��DP	�DP��DQ	�DQ��DR	�DR��DS	�DS��DT	�DT��DU	�DU��DV	�DV��DW	�DW��DX	�DX��DY	�DY��DZ	�DZ��D[	�D[��D\	�D\��D]	�D]��D^	�D^��D_	�D_��D`	�D`��Da	�Da��Db	�Db��Dc	�Dc��Dd	�Dd� De	�De��Df	�Df��Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj��Dk	�Dk��Dl	�Dl��Dm	�Dm��Dn	�Dn��Do	�Do��Dp	�Dp��Dq	�Dq��Dr	�Dr��Ds	�Ds��Dt	�Dt��Dt�Dy� D� D�4�D�t�D�њD� D�H D���D��3D�$�D�A�D�{3D���D��D�K3DڞgD�њD��D�Q�D�ngD�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��ÃÃA̅A�~�A�~�A�~�A�|�ÃÁÃẢ7A̋DA̋DA̍PA̍PA̍PA̋DẢ7Ả7Ả7A̍PȀ\A̓uA̍PA̅A̅A̅Ȧ+Ả7Ȧ+ÃA̅Ả7Ȧ+A�`BA�ZA�S�A�K�A�-A���A���A˥�Aˇ+A�x�A�p�A�dZA�ZA�M�A�/A�VA���A��A���AʬAʁA�5?A���Aɡ�A�XA���A�S�A�ĜA�|�A��
Aź^A���A��A��HA��yA�G�A���A�XA�x�A��/A�^5A�5?A��A�oA�
=A�1A���A�ffA���A��A��FA�ƨA�ZA���A���A��A�dZA�C�A�1A���A��9A��A���A��A��PA�ȴA�hsA��+A��A��uA���A� �A�t�A���A���A��A��A��uA��jA�v�A�dZA�r�A��!A��A�{A�/A���A|�yA|bAz�Ay�Ax�!Au�FAr��Ap�Ao?}AlbAi�TAg�Ad�HAax�A]?}AYG�AX-AVȴAV�HAV�\AR��AQ/AP�APZAN�DAL�RAK�wAK�^AK��AKt�AJr�AF��AD�DAD1ACx�AB��AA�FA@Q�A>{A:�yA9S�A85?A7�A6�A5C�A3��A1�hA0��A/��A.��A-?}A*$�A)��A(��A&�+A#\)A ��A  �A�A9XA
=A^5A�
A�7A�A��A�PA&�A��AȴA�\A�A�uAr�A�-A&�A��A�A1'A
=AA�A��A�Az�A��A��AbA
ĜA
�uA
��A
A�A	+A�wAƨA��An�AjAA�A n�@��H@�r�@�S�@��#@�Z@���@�v�@��h@�5?@�dZ@��@�V@�%@��/@�@�1@�t�@�
=@�=q@��@���@�9@���@ߕ�@�;d@�@�ff@ف@�`B@�M�@�\)@ܣ�@��@�
=@�33@�  @߾w@�dZ@�n�@��@�x�@��@��;@�$�@��`@�j@׍P@�?}@�p�@�;d@��y@�$�@��@ԛ�@�A�@�1'@�1@�{@�X@�O�@��/@�;d@�v�@���@���@¸R@���@�J@��@�Q�@�  @��@�33@��y@�M�@���@�G�@��@�dZ@�@���@��@�?}@���@��@�1@���@��@�bN@��F@���@���@��@���@�x�@�O�@��@��@�(�@��m@��y@���@�~�@�$�@��^@���@��m@��P@�o@��@��y@��y@���@���@���@�~�@�-@�J@�J@�@��@���@�x�@�X@��u@� �@��@���@�t�@�;d@��@���@�v�@��T@��7@�O�@��@�V@���@��`@��j@�  @�K�@���@�~�@�ff@�J@���@���@�X@�/@�%@���@���@�1'@��@�
=@���@�v�@�ff@�V@�E�@�@��7@�V@�%@��/@��j@��D@�bN@�I�@��@��
@�l�@�33@�@���@���@�$�@��-@�G�@���@���@��j@�9X@�1@��@���@�@�v�@�E�@�$�@��@�p�@�?}@���@���@��@�Z@�9X@�9X@�1'@��@���@�l�@�;d@��H@���@�V@���@��7@�`B@�&�@���@�Z@�b@���@�ƨ@��P@�l�@��@�ȴ@���@��+@�5?@���@��T@�p�@�7L@�/@�%@���@�r�@�b@��w@���@��@��@�K�@��H@��+@�M�@���@��@���@�G�@�/@�&�@��@��@�%@���@��/@���@�A�@� �@��@��@��P@�\)@�S�@�C�@��@��y@��!@��+@�ff@�M�@��@��@��u@uO�@kS�@a��@\��@Tz�@M�-@E�@:�!@5�@,��@(��@#��@@hs@{@r�@I�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ÃÃA̅A�~�A�~�A�~�A�|�ÃÁÃẢ7A̋DA̋DA̍PA̍PA̍PA̋DẢ7Ả7Ả7A̍PȀ\A̓uA̍PA̅A̅A̅Ȧ+Ả7Ȧ+ÃA̅Ả7Ȧ+A�`BA�ZA�S�A�K�A�-A���A���A˥�Aˇ+A�x�A�p�A�dZA�ZA�M�A�/A�VA���A��A���AʬAʁA�5?A���Aɡ�A�XA���A�S�A�ĜA�|�A��
Aź^A���A��A��HA��yA�G�A���A�XA�x�A��/A�^5A�5?A��A�oA�
=A�1A���A�ffA���A��A��FA�ƨA�ZA���A���A��A�dZA�C�A�1A���A��9A��A���A��A��PA�ȴA�hsA��+A��A��uA���A� �A�t�A���A���A��A��A��uA��jA�v�A�dZA�r�A��!A��A�{A�/A���A|�yA|bAz�Ay�Ax�!Au�FAr��Ap�Ao?}AlbAi�TAg�Ad�HAax�A]?}AYG�AX-AVȴAV�HAV�\AR��AQ/AP�APZAN�DAL�RAK�wAK�^AK��AKt�AJr�AF��AD�DAD1ACx�AB��AA�FA@Q�A>{A:�yA9S�A85?A7�A6�A5C�A3��A1�hA0��A/��A.��A-?}A*$�A)��A(��A&�+A#\)A ��A  �A�A9XA
=A^5A�
A�7A�A��A�PA&�A��AȴA�\A�A�uAr�A�-A&�A��A�A1'A
=AA�A��A�Az�A��A��AbA
ĜA
�uA
��A
A�A	+A�wAƨA��An�AjAA�A n�@��H@�r�@�S�@��#@�Z@���@�v�@��h@�5?@�dZ@��@�V@�%@��/@�@�1@�t�@�
=@�=q@��@���@�9@���@ߕ�@�;d@�@�ff@ف@�`B@�M�@�\)@ܣ�@��@�
=@�33@�  @߾w@�dZ@�n�@��@�x�@��@��;@�$�@��`@�j@׍P@�?}@�p�@�;d@��y@�$�@��@ԛ�@�A�@�1'@�1@�{@�X@�O�@��/@�;d@�v�@���@���@¸R@���@�J@��@�Q�@�  @��@�33@��y@�M�@���@�G�@��@�dZ@�@���@��@�?}@���@��@�1@���@��@�bN@��F@���@���@��@���@�x�@�O�@��@��@�(�@��m@��y@���@�~�@�$�@��^@���@��m@��P@�o@��@��y@��y@���@���@���@�~�@�-@�J@�J@�@��@���@�x�@�X@��u@� �@��@���@�t�@�;d@��@���@�v�@��T@��7@�O�@��@�V@���@��`@��j@�  @�K�@���@�~�@�ff@�J@���@���@�X@�/@�%@���@���@�1'@��@�
=@���@�v�@�ff@�V@�E�@�@��7@�V@�%@��/@��j@��D@�bN@�I�@��@��
@�l�@�33@�@���@���@�$�@��-@�G�@���@���@��j@�9X@�1@��@���@�@�v�@�E�@�$�@��@�p�@�?}@���@���@��@�Z@�9X@�9X@�1'@��@���@�l�@�;d@��H@���@�V@���@��7@�`B@�&�@���@�Z@�b@���@�ƨ@��P@�l�@��@�ȴ@���@��+@�5?@���@��T@�p�@�7L@�/@�%@���@�r�@�b@��w@���@��@��@�K�@��H@��+@�M�@���@��@���@�G�@�/@�&�@��@��@�%@���@��/@���@�A�@� �@��@��@��P@�\)@�S�@�C�@��@��y@��!@��+@�ffG�O�@��@��@��u@uO�@kS�@a��@\��@Tz�@M�-@E�@:�!@5�@,��@(��@#��@@hs@{@r�@I�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	�B	��B

=B
�B
�B
�B
$�B
(�B
0!B
B�B
XB
bNB
m�B
�B
��B
�LB
�B�BO�Be`B�hB�LB�B+B�B�B-BL�B[#Be`Br�B�JB��B��B��B��B��B��B��B��B��B�dB��BB��B�B�B��BPB�B�B'�B49B<jB>wB?}B>wB@�BF�B>wB&�B�B�^B�JB)�BBÖB�9B��Bu�B?}BhB
�B
�HB
ƨB
�bB
w�B
[#B
8RB
DB	�B	��B	�wB	�XB	�9B	��B	��B	��B	�%B	w�B	n�B	\)B	M�B	?}B	0!B	�B	B�B�B�B	%B	�B	\B	1B	%B	B��B��B�B�B�B�B�yB�;B�B��B��B��BÖB�jB�RB�3B�'B�!B�!B�B�B�B�'B�'B�-B�!B�'B�FB�?B�?B�?B�dBÖBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�
B�)B�TB�ZB�`B�yB�yB�HB�5B�/B�#B�B�B�/B�mB�yB�B�B�B�BB��B��BǮBɺBɺBɺB��B��B�)B�BB�HB�HB�HB�HB�;B�#B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺBȴBɺB��B�B�5B�fB�B��B	  B	B	1B	VB	\B	hB	{B	�B	{B	hB	JB	1B	
=B	1B	B	
=B	�B	 �B	�B	"�B	%�B	(�B	(�B	)�B	+B	)�B	)�B	'�B	$�B	!�B	�B	bB		7B	
=B	\B	\B	uB	�B	�B	#�B	%�B	(�B	.B	0!B	2-B	9XB	<jB	>wB	A�B	G�B	K�B	P�B	T�B	XB	\)B	cTB	ffB	gmB	gmB	ffB	ffB	ffB	ffB	gmB	hsB	jB	q�B	x�B	x�B	x�B	y�B	{�B	�B	�+B	�7B	�PB	�VB	�bB	�hB	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�9B	�LB	�RB	�XB	�dB	�dB	�jB	�wB	�wB	�}B	��B	��B	B	ÖB	ŢB	ǮB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�;B	�BB	�BB	�HB	�HB	�HB	�NB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B

=B
DB
DB
DB
JB
JB
JB
JB
PB
PB
VB
{B
�B
 �B
%�B
-B
0!B
7LB
<jB
B�B
J�B
O�B
W
B
ZB
_;B
dZB
hsB
k�B
o�B
t�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�"B	�zB	��B

:B
|B
�B
�B
$�B
(�B
0B
B�B
X	B
bGB
m�B
��B
��B
�FB
�B�BO�BeUB�\B�@B��BB�B�B- BL�B[BeVBr�B�<B��B��B��B��B��B��B��B��B��B�XB��B �B��B�B�B��BDB}B�B'�B4-B<`B>mB?tB>oB@zBF�B>mB&�B�B�RB�>B)�B�BÈB�)B��Bu�B?rBZB
�B
�@B
ƜB
�XB
w�B
[B
8HB
?B	�B	��B	�qB	�TB	�5B	��B	��B	��B	�!B	w�B	n�B	\&B	M�B	?|B	0"B	�B	B�B�B�B	&B	�B	ZB	4B	$B	B��B��B�B�B�B�B�|B�;B�B��B��B��B×B�mB�UB�7B�*B�'B�'B�B�B�B�*B�*B�0B�#B�+B�JB�BB�@B�CB�hBÚBȶB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�)B�PB�[B�`B�zB�xB�FB�6B�/B�"B�B�B�.B�lB�yB�B�B�B�BB��B��BǮBɹBɹBɻB��B��B�'B�>B�GB�EB�GB�GB�7B� B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺBȲBɹB��B� B�2B�bB�B��B��B	B	.B	QB	YB	fB	wB	}B	wB	fB	EB	.B	
:B	.B	B	
9B	�B	 �B	�B	"�B	%�B	(�B	(�B	)�B	*�B	)�B	)�B	'�B	$�B	!�B	�B	^B		4B	
:B	WB	YB	oB	�B	�B	#�B	%�B	(�B	.B	0B	2&B	9QB	<cB	>qB	A�B	G�B	K�B	P�B	T�B	XB	\!B	cMB	f]B	geB	ggB	f\B	f[B	f\B	f_B	gfB	hkB	jxB	q�B	x�B	x�B	x�B	y�B	{�B	�B	�"B	�/B	�GB	�LB	�ZB	�_B	�^B	�fB	�cB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	�	B	�B	�B	�-B	�CB	�GB	�NB	�\B	�[B	�`B	�lB	�mB	�rB	�yB	�xB	B	ÊB	ŖB	ǤB	ȩB	ɯB	ɯB	ɮB	ʴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�%B	�$B	�$B	�*B	�0B	�5B	�6B	�;B	�=B	�=B	�CB	�TB	�dB	�sB	�rB	�rB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
B
B
B
B
B
B
B
B
B
B
B
B
B
 B
B
%B
#B
#B
$B
	(B
	)B
	*B
	)B

.B
9B
7B
6B
<B
<B
<B
=B
BG�O�B
JB
mB
�B
 �B
%�B
-B
0B
7>B
<ZB
B�B
J�B
O�B
V�B
ZB
_-B
dKB
haB
kwB
o�B
t�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.15 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436352016080714363520160807143635  AO  ARCAADJP                                                                    20150502191705    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150502191705  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150502191705  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143635  IP                  G�O�G�O�G�O�                