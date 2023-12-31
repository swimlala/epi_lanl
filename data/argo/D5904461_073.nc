CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-09-22T09:16:53Z AOML 3.0 creation; 2016-08-07T21:36:39Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150922091653  20160807143639  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               IA   AO  5286_8897_073                   2C  D   APEX                            6531                            072314                          846 @�qC�Q	1   @�qD�m�F@3I�^5?}�c5&�x��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    IA   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�  B���B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C��C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D ��D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�fD���D�S3D��fD��3D�  D�9�D�� D��3D��D�33D���D��fD�fD�33Dڀ D�� D�	�D�@ D�|�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@���AffA"ffABffAbffA�33A�33A�33A�33A�33A�33A�33A�33B ��B��B��B��B ��B(��B0��B8��B@��BI  BP��BX��B`34Bh��Bp��Bx��B�L�B�L�B�L�B�L�B��B�L�B�L�B�L�B�L�B�L�B�L�B�� B�� B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�BԀ Bس3B�L�B��B��B�L�B�L�B�L�B�L�B�L�B�L�C &fC&fC&fC&fC&fC
&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC &fC"&fC$&fC&&fC(&fC*&fC,@ C.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd&fCf&fCh&fCj&fCl&fCn&fCp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�  C�  C�fC�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D ��D	�D��D D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D		�D	��D
	�D
��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D 	�D ��D!4D!��D"	�D"��D#	�D#��D$	�D$��D%	�D%��D&	�D&��D'	�D'��D(	�D(��D)	�D)��D*	�D*��D+	�D+��D,	�D,��D-	�D-��D.	�D.��D/	�D/��D0	�D0��D1	�D1��D2	�D2��D3	�D3��D4	�D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8��D9	�D9��D:	�D:��D;	�D;��D<	�D<�4D=	�D=��D>	�D>��D?	�D?��D@	�D@��DA	�DA��DB	�DB��DC	�DC��DD	�DD��DE	�DE��DF	�DF��DG	�DG��DH	�DH��DI	�DI��DJ	�DJ��DK	�DK��DL	�DL��DM	�DM��DN	�DN��DO	�DO��DP	�DP��DQ	�DQ��DR	�DR��DS	�DS��DT DT��DU	�DU��DV	�DV��DW	�DW��DX	�DX��DY	�DY��DZ	�DZ��D[	�D[��D\	�D\��D]	�D]��D^	�D^��D_	�D_��D`	�D`��Da	�Da��Db	�Db��Dc	�Dc��Dd	�Dd��De	�De��Df	�Df��Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj��Dk	�Dk��Dl	�Dl��Dm	�Dm��Dn	�Dn��Do	�Do��Dp	�Dp��Dq	�Dq��Dr	�Dr��Ds	�Ds��Dt	�Dt��Dt� Dy� D��D�X D��3D�� D��D�>gD���D�� D�gD�8 D��gD��3D�3D�8 Dڄ�D���D�gD�D�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�XA�ZA�Q�A�ZA�`BA�\)A�VA�S�A�Q�A�;dA��A�
=A�A���A���A���A��A��A��A��A��mA��A���A�ȴA�wA��A��hA�E�A�K�A�VAݶFA���AڬA��/A�^5A���A�O�A��Aщ7A�AЇ+A�ffA���AʃAƶFA���A�A�`BAİ!Aĝ�A�1'A�r�A�t�A���A��TA��A��hA�9XA���A�r�A���A���A�E�A�5?A��A�p�A��A�~�A���A���A�/A���A�v�A�{A�hsA�
=A�p�A���A��A��
A�  A�t�A��`A��TA��^A��PA���A�S�A� �A�7LA�hsA�VA�t�A���A�?}A�%A���A�&�A�1'A�;dA�dZA��PA�v�A��/A��A��uA�A�A�dZA�v�A��/A�A�A���A�n�A�ZA��PA��+A���A���A��A�\)A�%A��RAG�A~M�A{\)Ay�;Aw�-ArQ�Ad  A\~�AX��AV{AS�AR�!AP�\AN��AN  AM�^AL9XAJĜAJ��AJĜAI�AH9XAG�PAF-AD��AC��AC�AC��AB�A@(�A<^5A:��A9��A9VA8��A6^5A0��A-��A,JA(��A&�yA&~�A&r�A&=qA$VA"�A!O�A Q�A M�A M�A Q�A -A bA!�A �A 5?AĜA�;A~�AM�A��A{A��A�7A7LA��A��A�^A��A��A  A��A;dA��A��A
�A
��A
VA	��A	��A	l�A	+A	oA^5AjA�A33A�\A��A�A�PA 1'@�^5@���@��P@�V@��H@��D@���@��h@��@�D@�X@���@�^5@���@�
=@�v�@�ƨ@�;d@���@�u@�A�@�?}@�M�@ꗍ@�p�@�;d@���@�=q@�E�@◍@��
@���@��H@��@�X@�n�@��T@�@�{@�  @ܓu@���@��@�  @ٙ�@��@�{@۶F@� �@۝�@�V@�j@Ԭ@�O�@�X@ՙ�@��@��#@�`B@���@Լj@ԓu@�C�@��@���@Ͼw@Ͼw@�Z@�X@�v�@��m@Լj@�O�@�(�@ӥ�@Ӯ@ӝ�@�ȴ@�$�@�J@ѩ�@���@�+@Χ�@ͩ�@�hs@�+@�-@���@̋D@���@�Ĝ@�Ĝ@̃@�`B@Ͳ-@��@�&�@̃@�1'@�;d@��H@ʗ�@ʸR@��H@���@ɲ-@�&�@�r�@���@�dZ@�"�@��@���@őh@��`@�9X@��m@Õ�@�
=@��y@��y@���@¸R@�v�@�O�@��/@���@���@���@� �@�t�@��@��@�v�@���@�X@�/@��@���@��@��;@�K�@���@���@���@�dZ@�~�@�O�@��`@��j@��@�n�@�M�@�n�@�-@�J@���@��@�x�@��@�X@��`@�j@�A�@��@��F@���@�{@��@�l�@�@��R@�ff@�@�hs@���@�X@�p�@�hs@�O�@�?}@��D@���@�|�@���@�A�@�ȴ@�$�@��#@��@��@��@�C�@��R@���@��u@��@�|�@�|�@�|�@��@�|�@�|�@�|�@��P@��@���@��u@��D@���@���@��u@� �@��
@�K�@��@�E�@��@��#@�hs@��@��m@�;d@�5?@��@���@�^5@�\)@��T@�=q@�-@�`B@��u@��F@��@�;d@��H@��\@��@��@�-@�ff@�V@���@�`B@�I�@�K�@�-@���@�`B@�`B@�?}@�?}@���@�r�@��j@�r�@���@��P@���@��w@���@��P@��P@��@��@��@�S�@�^5@��-@���@���@�Ĝ@���@�?}@��\@�b@v{@lI�@f5?@]�h@TI�@K�F@BM�@;ƨ@6v�@2^5@-p�@'�;@#��@{@�\@@�#@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�XA�ZA�Q�A�ZA�`BA�\)A�VA�S�A�Q�A�;dA��A�
=A�A���A���A���A��A��A��A��A��mA��A���A�ȴA�wA��A��hA�E�A�K�A�VAݶFA���AڬA��/A�^5A���A�O�A��Aщ7A�AЇ+A�ffA���AʃAƶFA���A�A�`BAİ!Aĝ�A�1'A�r�A�t�A���A��TA��A��hA�9XA���A�r�A���A���A�E�A�5?A��A�p�A��A�~�A���A���A�/A���A�v�A�{A�hsA�
=A�p�A���A��A��
A�  A�t�A��`A��TA��^A��PA���A�S�A� �A�7LA�hsA�VA�t�A���A�?}A�%A���A�&�A�1'A�;dA�dZA��PA�v�A��/A��A��uA�A�A�dZA�v�A��/A�A�A���A�n�A�ZA��PA��+A���A���A��A�\)A�%A��RAG�A~M�A{\)Ay�;Aw�-ArQ�Ad  A\~�AX��AV{AS�AR�!AP�\AN��AN  AM�^AL9XAJĜAJ��AJĜAI�AH9XAG�PAF-AD��AC��AC�AC��AB�A@(�A<^5A:��A9��A9VA8��A6^5A0��A-��A,JA(��A&�yA&~�A&r�A&=qA$VA"�A!O�A Q�A M�A M�A Q�A -A bA!�A �A 5?AĜA�;A~�AM�A��A{A��A�7A7LA��A��A�^A��A��A  A��A;dA��A��A
�A
��A
VA	��A	��A	l�A	+A	oA^5AjA�A33A�\A��A�A�PA 1'@�^5@���@��P@�V@��H@��D@���@��h@��@�D@�X@���@�^5@���@�
=@�v�@�ƨ@�;d@���@�u@�A�@�?}@�M�@ꗍ@�p�@�;d@���@�=q@�E�@◍@��
@���@��H@��@�X@�n�@��T@�@�{@�  @ܓu@���@��@�  @ٙ�@��@�{@۶F@� �@۝�@�V@�j@Ԭ@�O�@�X@ՙ�@��@��#@�`B@���@Լj@ԓu@�C�@��@���@Ͼw@Ͼw@�Z@�X@�v�@��m@Լj@�O�@�(�@ӥ�@Ӯ@ӝ�@�ȴ@�$�@�J@ѩ�@���@�+@Χ�@ͩ�@�hs@�+@�-@���@̋D@���@�Ĝ@�Ĝ@̃@�`B@Ͳ-@��@�&�@̃@�1'@�;d@��H@ʗ�@ʸR@��H@���@ɲ-@�&�@�r�@���@�dZ@�"�@��@���@őh@��`@�9X@��m@Õ�@�
=@��y@��y@���@¸R@�v�@�O�@��/@���@���@���@� �@�t�@��@��@�v�@���@�X@�/@��@���@��@��;@�K�@���@���@���@�dZ@�~�@�O�@��`@��j@��@�n�@�M�@�n�@�-@�J@���@��@�x�@��@�X@��`@�j@�A�@��@��F@���@�{@��@�l�@�@��R@�ff@�@�hs@���@�X@�p�@�hs@�O�@�?}@��D@���@�|�@���@�A�@�ȴ@�$�@��#@��@��@��@�C�@��R@���@��u@��@�|�@�|�@�|�@��@�|�@�|�@�|�@��P@��@���@��u@��D@���@���@��u@� �@��
@�K�@��@�E�@��@��#@�hs@��@��m@�;d@�5?@��@���@�^5@�\)@��T@�=q@�-@�`B@��u@��F@��@�;d@��H@��\@��@��@�-@�ff@�V@���@�`B@�I�@�K�@�-@���@�`B@�`B@�?}@�?}@���@�r�@��j@�r�@���@��P@���@��w@���@��P@��P@��@��@��@�S�@�^5@��-@���@���@�ĜG�O�@�?}@��\@�b@v{@lI�@f5?@]�h@TI�@K�F@BM�@;ƨ@6v�@2^5@-p�@'�;@#��@{@�\@@�#@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�5B
�5B
�/B
�5B
�5B
�5B
�5B
�/B
�)B
�#B
�B
�B
�B
�B
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
�
B
�/B
�B�BQ�BYB]/B_;B[#BS�BM�BYBp�B�B�3B�^B��B��B�BN�BZB]/Be`Bv�Bv�B~�B�\B��BĜBDB�B,B)�B$�B �B�B\B1B+B	7B	7BoB�BhBVBDBBBBDB��B�NB��B��B�)B��BƨB�FB��B��B�Bw�B[#BH�B;dB�B+B��B�B��B��B�?B��B��B�BXBO�BE�BE�BE�BD�B@�B5?B�BPBB
�B
��B
�}B
�-B
�oB
�B
|�B
o�B
bNB
N�B
<jB
)�B
 �B
DB	��B	�B	�jB	T�B	!�B	JB��B��B�B�`B�/B�B��B��BƨBŢBÖB��B�^B�FB�?B�3B�'B�B�B��B��B�oB�JB�7B�%B�B|�Bu�B�%B�PB|�Bs�Bs�Bs�Br�Br�Bp�Bq�Bw�B~�B�B�B�PB�{B�jBBŢBȴBƨBBB�#B�B�sB�;B��B��B��B�
BǮB�wB��B�}B��BĜB��B��B��BǮB��B��B��B��B��B��B�B�B�B�B�B�
B��BɺB�XB�3B�3B�LBĜB�B�5B�B�B�HB��B�B�B�yB��B��B�HB�B��B	uB	�B	%�B	1'B	6FB	6FB	,B	�B	�B	$�B	-B	7LB	0!B	$�B	"�B	/B	"�B	"�B	&�B	/B	@�B	C�B	F�B	O�B	Q�B	H�B	H�B	Q�B	`BB	ffB	cTB	]/B	VB	I�B	O�B	VB	YB	bNB	dZB	cTB	bNB	bNB	e`B	cTB	bNB	iyB	cTB	cTB	iyB	r�B	|�B	�1B	�\B	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	�uB	�JB	�=B	�1B	�1B	��B	�{B	�oB	�uB	��B	��B	��B	��B	�B	�B	�!B	�'B	�B	�B	�!B	�-B	�3B	�FB	�LB	�LB	�LB	�jB	��B	�}B	�}B	��B	B	�}B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	�}B	�wB	�wB	��B	ĜB	ƨB	ɺB	ȴB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	ɺB	ɺB	ȴB	ƨB	B	�}B	�wB	�XB	�RB	�XB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�B	�B	�#B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ĜB	�wB	�jB	�jB	�jB	�dB	�^B	�LB	�LB	�?B	�9B	�3B	�9B	�9B	�?B	�LB	�RB	�XB	�XB	�^B	�jB	�}B	ĜB	ŢB	ƨB	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	ǮB	ȴB	ɺB	��B	��B	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
\B	�B	��B
1B
hB
�B
+B
5?B
5?B
;dB
B�B
H�B
L�B
P�B
T�B
ZB
^5B
cTB
gmB
l�B
p�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�,B
�,B
�&B
�.B
�)B
�-B
�-B
�$B
�B
�B
�B
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
��B
��B
�B
�%B
�B�BQ�BYB]$B_0B[BS�BM�BYBp�B�B�&B�QB��B��B�BN�BZB]BeVBv�Bv�B~�B�NB��BĒB8B�B,B)�B$�B �B�BUB(B!B	)B	,BfBtB]BHB:BB B �B;B��B�AB��B��B�B��BƚB�:B��B�vB�Bw�B[BH�B;TB�BB��B�B��B̿B�1B��B�~B��BXBO�BE�BE�BE�BD�B@vB5/B�BBB �B
�B
��B
�tB
�"B
�fB
�B
|�B
o�B
bEB
N�B
<^B
)�B
 �B
=B	��B	�B	�dB	T�B	!�B	IB��B��B�B�bB�1B�B�B��BƪBŦBÛB��B�bB�KB�DB�8B�,B�!B�B��B��B�uB�PB�=B�)B�B|�Bu�B�*B�UB|�Bs�Bs�Bs�Br�Br�Bp�Bq�Bw�B~�B�B�B�TB�~B�mBBŤBȷBƨBBB�#B�B�qB�<B��B��B��B�	BǰB�yB��B�}B��BğB��B��B��BǯB��B��B��B��B��B��B�B�B�B�B�B�
B��BɺB�XB�4B�5B�LBĜB�B�4B�B�B�HB��B�B�B�wB��B��B�EB�B��B	rB	�B	%�B	1#B	6BB	6AB	,B	�B	�B	$�B	-
B	7GB	0B	$�B	"�B	/B	"�B	"�B	&�B	/B	@|B	C�B	F�B	O�B	Q�B	H�B	H�B	Q�B	`;B	f]B	cMB	])B	U�B	I�B	O�B	U�B	YB	bEB	dSB	cMB	bFB	bFB	eYB	cNB	bFB	irB	cLB	cLB	ipB	r�B	|�B	�)B	�SB	�eB	�cB	�lB	��B	��B	��B	��B	��B	��B	�kB	�AB	�5B	�)B	�)B	��B	�sB	�gB	�lB	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�+B	�;B	�AB	�BB	�AB	�bB	�yB	�sB	�sB	�yB	B	�rB	�qB	��B	�{B	�~B	�B	�yB	�yB	�yB	�xB	�qB	�mB	�oB	�yB	ĒB	ƞB	ɯB	ȩB	ǣB	ǦB	ȪB	ɯB	ʷB	ʶB	ʷB	ʵB	ʸB	ʵB	ɰB	ɮB	ȧB	ƝB	B	�rB	�mB	�PB	�GB	�MB	�fB	ʶB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	˻B	˽B	˽B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ēB	�lB	�aB	�`B	�^B	�[B	�TB	�AB	�BB	�3B	�/B	�)B	�,B	�/B	�4B	�@B	�FB	�NB	�OB	�TB	�_B	�pB	ĒB	ŗB	ƜB	˺B	��B	��B	ʶB	ɮB	˺B	��B	��B	��B	��B	��B	˼B	ʷB	ǤB	ȩB	ɰB	��B	��B	�UB	�gB	�wB	�B	�B	�B	�B	�B	�B	�B	�xB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�B	��B
&B
YB
�B
*�B
52B
50B
;VB
B�B
H�B
L�B
P�B
T�B
ZB
^&B
cCB
g_B
l~B
p�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.15 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436392016080714363920160807143639  AO  ARCAADJP                                                                    20150922091653    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150922091653  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150922091653  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143639  IP                  G�O�G�O�G�O�                