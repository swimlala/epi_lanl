CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-04-17T02:16:29Z AOML 3.0 creation; 2016-08-07T21:36:34Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150417021629  20160807143634  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               +A   AO  5286_8897_043                   2C  D   APEX                            6531                            072314                          846 @�I�&~�1   @�I����	@2���"���c�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    +A   B   B   @�33@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtL�Dy� D��D�I�D���D��fD��fD�6fD�|�D���D��D�FfD���D���D���D�@ Dډ�D�ٚD��3D�9�D�l�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���AffA"ffAD  AbffA�33A�33A�33A�33A�33A�33A�33A�33B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C &fC&fC&fC&fC&fC
&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC &fC"&fC$&fC&&fC(&fC*&fC,&fC.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd&fCf&fCh&fCj&fCl&fCn&fCp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D ��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D D��D	�D��D		�D	��D
	�D
��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D 	�D ��D!	�D!��D"	�D"��D#	�D#��D$	�D$��D%	�D%��D&	�D&��D'	�D'��D(	�D(��D)	�D)��D*	�D*��D+	�D+��D,	�D,��D-	�D-��D.	�D.��D/	�D/��D0	�D0��D1	�D1��D2	�D2��D3	�D3��D4	�D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8��D9	�D9��D:	�D:��D;	�D;��D<	�D<��D=	�D=��D>	�D>��D?	�D?��D@	�D@��DA	�DA��DB	�DB��DC	�DC��DD	�DD��DE	�DE��DF	�DF��DG	�DG��DH	�DH��DI	�DI��DJ	�DJ��DK	�DK��DL	�DL��DM	�DM��DN	�DN��DO	�DO��DP	�DP��DQ	�DQ��DR	�DR��DS	�DS��DT	�DT��DU	�DU��DV	�DV��DW	�DW��DX	�DX��DY	�DY��DZ	�DZ��D[	�D[��D\	�D\��D]	�D]��D^	�D^��D_	�D_��D`	�D`��Da	�Da��Db	�Db��Dc	�Dc��Dd	�Dd��De	�De��Df	�Df��Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj��Dk	�Dk��Dl	�Dl��Dm	�Dm��Dn	�Dn��Do	�Do��Dp	�Dp��Dq	�Dq��Dr	�Dr��Ds	�Ds��Dt	�DtVgDy��D�!�D�NgD��gD��3D��3D�;3D���D��D��D�K3D��gD��D��gD�D�DڎgD��gD�� D�>gD�q�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�XA�\)A�^5A�^5A�^5A͓uA�ƨA��
A��mA��TA���AͮA͇+A�O�A�7LA��A��ÃA�+A�$�A��A�bA���A��A��;A���A���A�n�A�jA��;A�ĜA���Aɗ�A�ffA�oA���A���Aȥ�Aȟ�A�JA�%A�~�Aǩ�A�~�A�bNAƴ9A���A�|�A�ƨA���A��A��/Aç�Aã�AÓuA���A�VA�M�A�1A��A�~�A�33A��yA�z�A�
=A���A�%A�%A�"�A��FA��yA�33A�A��#A�O�A��FA�+A�\)A�E�A���A��TA��FA�+A�dZA�
=A��A�1'A��7A���A���A�ZA��TA��DA��hA�O�A�1'A�  A��PA� �A�Q�A���A��7A��A���A���A��A��uA��PA��uA�ȴA�-A�ƨA�~�A���A��RA��A�I�A�p�A���A�A�A���A���A��A�+A��A��A�33A|��Az�HAy�#AxAv1An��Ah��Afv�Ac33AX�jAR�yAQ�AQXAOl�AK��AI�AI
=ADĜABE�A@��A@=qA?�A?%A>r�A=��A<�A9K�A81A7��A7|�A5��A4��A1�A/��A/�FA-�A,I�A+S�A*�A*r�A*��A+33A*��A(z�A&5?A%��A&JA&n�A&v�A& �A%\)A$�\A#ƨA"�A"I�A!�hA jA�yA��A^5A��A^5A  A33A~�A$�A1'A��AVA�-A�A�A�yAG�AG�A�yAz�AI�A�AVA/An�A��A��Al�A�PAp�A
�A��A`BA�A  AJAffAI�A�A"�A?}AAC�A ~�A @��T@�z�@�|�@�\)@�\)@�=q@��@�E�@��@��@�v�@�E�@�/@���@�7L@�Ĝ@���@�K�@�7L@ա�@�=q@���@�J@�-@�~�@ҟ�@ҏ\@ҧ�@ҏ\@�ȴ@�C�@�o@�@�ƨ@�A�@�I�@� �@��
@�33@�"�@�C�@ёh@��@ϝ�@��@�-@�/@��;@���@Ɂ@�Ĝ@�z�@�A�@��
@�C�@�{@Ł@���@�Q�@Å@��y@�J@�7L@���@�A�@��
@�dZ@�+@��y@�^5@�-@�$�@�{@���@�hs@�X@�O�@�/@��@���@�z�@�9X@��@�  @�  @��m@���@�dZ@�33@���@��@���@�ȴ@�ȴ@��H@��@���@�{@���@��h@�G�@��@��/@�r�@�bN@�Q�@�1'@� �@�b@��;@��@���@�S�@�@���@�ȴ@���@���@��+@��+@�~�@�v�@�{@�@��@���@���@��7@��@�x�@�p�@�p�@�p�@�`B@��@�bN@�l�@���@�ff@�^5@��^@�@���@�O�@��@�&�@�?}@�/@�V@�%@��@�Ĝ@���@�(�@�K�@�ȴ@���@�n�@�V@�$�@���@��@���@�j@��@�9X@��F@��@�dZ@�S�@�33@�
=@��@��!@�-@���@���@�x�@�hs@�X@�?}@���@���@�bN@� �@��;@��P@�S�@�;d@�33@��\@���@�1'@��w@�\)@��@���@�ff@�-@�@��`@�I�@��;@�|�@�o@��@���@�v�@��@��@���@��j@��@� �@��w@�C�@���@��@��@��`@��u@�j@��m@���@���@��P@�t�@�S�@�o@�@���@�v�@��@��T@��#@���@��-@�7L@�V@���@���@��u@�1'@��;@�ƨ@��F@���@�dZ@�@��+@�M�@�=q@�=q@�5?@���@��@�X@�?}@��@�v�@��@�C�@}��@tZ@j��@_�@U�@M`B@F��@@A�@6ff@01'@)��@%`B@ bN@�!@�@o@ȴ@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�XA�\)A�^5A�^5A�^5A͓uA�ƨA��
A��mA��TA���AͮA͇+A�O�A�7LA��A��ÃA�+A�$�A��A�bA���A��A��;A���A���A�n�A�jA��;A�ĜA���Aɗ�A�ffA�oA���A���Aȥ�Aȟ�A�JA�%A�~�Aǩ�A�~�A�bNAƴ9A���A�|�A�ƨA���A��A��/Aç�Aã�AÓuA���A�VA�M�A�1A��A�~�A�33A��yA�z�A�
=A���A�%A�%A�"�A��FA��yA�33A�A��#A�O�A��FA�+A�\)A�E�A���A��TA��FA�+A�dZA�
=A��A�1'A��7A���A���A�ZA��TA��DA��hA�O�A�1'A�  A��PA� �A�Q�A���A��7A��A���A���A��A��uA��PA��uA�ȴA�-A�ƨA�~�A���A��RA��A�I�A�p�A���A�A�A���A���A��A�+A��A��A�33A|��Az�HAy�#AxAv1An��Ah��Afv�Ac33AX�jAR�yAQ�AQXAOl�AK��AI�AI
=ADĜABE�A@��A@=qA?�A?%A>r�A=��A<�A9K�A81A7��A7|�A5��A4��A1�A/��A/�FA-�A,I�A+S�A*�A*r�A*��A+33A*��A(z�A&5?A%��A&JA&n�A&v�A& �A%\)A$�\A#ƨA"�A"I�A!�hA jA�yA��A^5A��A^5A  A33A~�A$�A1'A��AVA�-A�A�A�yAG�AG�A�yAz�AI�A�AVA/An�A��A��Al�A�PAp�A
�A��A`BA�A  AJAffAI�A�A"�A?}AAC�A ~�A @��T@�z�@�|�@�\)@�\)@�=q@��@�E�@��@��@�v�@�E�@�/@���@�7L@�Ĝ@���@�K�@�7L@ա�@�=q@���@�J@�-@�~�@ҟ�@ҏ\@ҧ�@ҏ\@�ȴ@�C�@�o@�@�ƨ@�A�@�I�@� �@��
@�33@�"�@�C�@ёh@��@ϝ�@��@�-@�/@��;@���@Ɂ@�Ĝ@�z�@�A�@��
@�C�@�{@Ł@���@�Q�@Å@��y@�J@�7L@���@�A�@��
@�dZ@�+@��y@�^5@�-@�$�@�{@���@�hs@�X@�O�@�/@��@���@�z�@�9X@��@�  @�  @��m@���@�dZ@�33@���@��@���@�ȴ@�ȴ@��H@��@���@�{@���@��h@�G�@��@��/@�r�@�bN@�Q�@�1'@� �@�b@��;@��@���@�S�@�@���@�ȴ@���@���@��+@��+@�~�@�v�@�{@�@��@���@���@��7@��@�x�@�p�@�p�@�p�@�`B@��@�bN@�l�@���@�ff@�^5@��^@�@���@�O�@��@�&�@�?}@�/@�V@�%@��@�Ĝ@���@�(�@�K�@�ȴ@���@�n�@�V@�$�@���@��@���@�j@��@�9X@��F@��@�dZ@�S�@�33@�
=@��@��!@�-@���@���@�x�@�hs@�X@�?}@���@���@�bN@� �@��;@��P@�S�@�;d@�33@��\@���@�1'@��w@�\)@��@���@�ff@�-@�@��`@�I�@��;@�|�@�o@��@���@�v�@��@��@���@��j@��@� �@��w@�C�@���@��@��@��`@��u@�j@��m@���@���@��P@�t�@�S�@�o@�@���@�v�@��@��T@��#@���@��-@�7L@�V@���@���@��u@�1'@��;@�ƨ@��F@���@�dZ@�@��+@�M�@�=q@�=q@�5?@���@��@�X@�?}G�O�@�v�@��@�C�@}��@tZ@j��@_�@U�@M`B@F��@@A�@6ff@01'@)��@%`B@ bN@�!@�@o@ȴ@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBȴBȴBȴBȴB��B�ZB	B	uB	(�B	8RB	A�B	L�B	W
B	_;B	bNB	ffB	t�B	�JB	��B	��B	��B	��B	��B	��B	��B	��B	�wB	�B
B�B
C�B
G�B
�1B
�?B
�yBVB!�B2-B>wBN�BiyB�bB�'B��B�B�sB�)B�HB�B�B,BH�B`BBffBgmBhsBn�Br�Bw�Bw�B}�B�B�B�%B�B� Bt�BgmB?}B1'B(�B33B7LB9XBS�Bo�B�1B��Bl�BcTB_;Bo�B� B�PB{�BP�BR�BF�B8RB�BoB?}B1'B�
B�jB��B�\B�Br�Bl�B\)Be`Bq�B�B��B��B�+Bt�BdZBK�B>wB1'B'�B �BPBB
��B
�5B
��B
��B
�!B
��B
�1B
z�B
cTB
R�B
?}B
)�B
bB
B	��B	�B	��B	��B	n�B	[#B	K�B		7B�B�`B�NB�B��BǮB��B�^B�XB�LB�LB�RB�FB�?B�-B�B�-B�3B�3B�?B�RB�FB�RB�RB��BƨB��B��B��B��B�
B�HB�mB�fB�)B�BB�B��B��B	B		7B	DB	PB	PB	JB	JB	VB	uB	uB	oB	�B	9XB	<jB	:^B	<jB	?}B	D�B	B�B	<jB	@�B	B�B	C�B	L�B	YB	\)B	^5B	_;B	_;B	^5B	[#B	YB	W
B	VB	XB	YB	W
B	P�B	@�B	!�B	hB	{B	/B	=qB	I�B	J�B	I�B	E�B	C�B	?}B	<jB	;dB	;dB	;dB	:^B	:^B	=qB	?}B	D�B	F�B	C�B	E�B	E�B	L�B	M�B	I�B	<jB	1'B	�B	B�B�mB�HB�sB�B�B��B��B	B	VB	oB	uB	�B	!�B	!�B	!�B	'�B	-B	-B	-B	-B	.B	1'B	1'B	8RB	7LB	6FB	8RB	8RB	7LB	9XB	<jB	F�B	L�B	N�B	O�B	Q�B	T�B	ZB	\)B	_;B	cTB	gmB	jB	n�B	r�B	s�B	u�B	u�B	v�B	w�B	w�B	{�B	� B	�B	�%B	�+B	�1B	�1B	�1B	�7B	�7B	�DB	�PB	�VB	�VB	�VB	�VB	�bB	�hB	�oB	�oB	�oB	�uB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�9B	�FB	�LB	�LB	�RB	�RB	�RB	�RB	�RB	�RB	�^B	�dB	�dB	�jB	�dB	�dB	�qB	�qB	�}B	��B	B	ĜB	ĜB	ĜB	ĜB	ĜB	ÖB	ĜB	ĜB	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�5B	�HB	�NB	�TB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
VB
�B
�B
"�B
%�B
/B
6FB
<jB
B�B
F�B
N�B
T�B
[#B
^5B
bNB
ffB
jB
n�B
r�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   BȿB��BȿBȿB��B�eB	B	{B	(�B	8WB	A�B	L�B	W
B	_<B	bQB	fjB	t�B	�JB	��B	��B	��B	��B	��B	��B	��B	��B	�yB	�B
B�B
C�B
G�B
�(B
�6B
�nBMB!�B2!B>mBN�BimB�XB�B��B�B�fB�B�<B�qB�B+�BH�B`4BfXBg]BhkBn�Br�Bw�Bw�B}�B��B�B�B�B�Bt�BgaB?rB1B(�B3#B7@B9HBS�Bo�B�&B��Bl|BcEB_,Bo�B�B�DB{�BP�BR�BF�B8CB�BbB?oB1B��B�ZB��B�MB�Br�BlB\BePBq�B��B��B��B�Bt�BdKBK�B>kB1B'�B �BDBB
��B
�+B
��B
�|B
�B
�zB
�'B
z�B
cIB
R�B
?uB
)�B
[B
B	��B	�~B	��B	��B	n�B	[!B	K�B		8B�B�bB�QB�!B��BǳB��B�dB�\B�PB�OB�UB�IB�CB�1B�B�0B�6B�5B�AB�TB�JB�TB�TB��BƨB��B��B��B��B�B�LB�mB�gB�)B�CB�B��B��B	B		6B	BB	NB	OB	IB	IB	SB	sB	sB	mB	�B	9TB	<eB	:YB	<fB	?wB	D�B	B�B	<fB	@�B	B�B	C�B	L�B	YB	\%B	^0B	_6B	_5B	^/B	[ B	YB	WB	U�B	X
B	YB	WB	P�B	@}B	!�B	dB	yB	/B	=jB	I�B	J�B	I�B	E�B	C�B	?yB	<fB	;]B	;`B	;_B	:YB	:ZB	=lB	?{B	D�B	F�B	C�B	E�B	E�B	L�B	M�B	I�B	<eB	1"B	�B	B�B�kB�FB�qB�B�B��B��B	B	RB	jB	nB	�B	!�B	!�B	!�B	'�B	-B	-B	-	B	-B	.B	1 B	1 B	8LB	7DB	6AB	8LB	8KB	7FB	9RB	<bB	F�B	L�B	N�B	O�B	Q�B	T�B	ZB	\#B	_3B	cLB	gfB	jxB	n�B	r�B	s�B	u�B	u�B	v�B	w�B	w�B	{�B	�B	�B	�B	�!B	�'B	�+B	�+B	�-B	�-B	�<B	�GB	�KB	�NB	�LB	�LB	�\B	�\B	�gB	�dB	�fB	�kB	�hB	�kB	�jB	�tB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�/B	�;B	�AB	�AB	�HB	�HB	�HB	�IB	�IB	�FB	�TB	�ZB	�XB	�^B	�[B	�ZB	�eB	�gB	�oB	�zB	B	ēB	ēB	ďB	ĐB	ĐB	ÊB	ďB	ĐB	đB	ƛB	ȫB	ɮB	ʶB	˼B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�+B	�>B	�AB	�IB	�_B	�oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�G�O�B
B
JB
xB
�B
"�B
%�B
/B
66B
<ZB
B�B
F�B
N�B
T�B
[B
^#B
b?B
fWB
joB
n�B
r�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.15 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436342016080714363420160807143634  AO  ARCAADJP                                                                    20150417021629    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150417021629  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150417021629  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143634  IP                  G�O�G�O�G�O�                