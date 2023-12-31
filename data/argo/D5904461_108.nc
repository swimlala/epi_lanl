CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-03-24T19:49:10Z AOML 3.0 creation; 2016-08-07T21:36:45Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160324194910  20160807143645  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               lA   AO  5286_8897_108                   2C  D   APEX                            6531                            072314                          846 @ןm��T�1   @ןn'Ҕ.@2�+I��cV�Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    lA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*�C,�C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D6��D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dx��D��D�FfD�� D��fD��D�FfD�y�D�� D�fD�I�D�Y�D��3D���D�33Dڠ D�� D� D�L�D�vfD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���AffA"ffABffAbffA�33A�33A�33A�33A�33A�33A�33A�33B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�BĀ BȀ B�L�B��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�� B��B�L�B�L�C &fC&fC&fC&fC&fC
&fC&fC&fC&fC&fC@ C&fC&fC&fC&fC&fC &fC"&fC$&fC&&fC(&fC*@ C,@ C.@ C0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb@ Cd&fCf&fCh&fCj&fCl&fCn&fCp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D ��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D		�D	��D
	�D
��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D 	�D ��D!	�D!��D"	�D"��D#	�D#��D$	�D$��D%	�D%��D&	�D&��D'	�D'��D(	�D(��D)	�D)��D*	�D*��D+	�D+��D,	�D,��D-	�D-��D.	�D.��D/	�D/��D0	�D0��D1	�D1��D2	�D2��D3	�D3��D4	�D4��D5	�D5��D6	�D6��D74D7��D8	�D8��D9	�D9��D:	�D:��D;	�D;��D<	�D<��D=	�D=��D>	�D>��D?	�D?��D@	�D@��DA	�DA��DB	�DB��DC	�DC��DD	�DD��DE	�DE��DF	�DF��DG	�DG��DH	�DH��DI	�DI� DJ	�DJ��DK	�DK��DL	�DL��DM	�DM��DN	�DN��DO	�DO��DP	�DP��DQ	�DQ��DR	�DR��DS	�DS��DT	�DT��DU	�DU��DV	�DV��DW	�DW��DX	�DX��DY	�DY��DZ	�DZ��D[	�D[��D\	�D\��D]	�D]��D^	�D^��D_	�D_��D`	�D`��Da	�Da��Db	�Db��Dc	�Dc��Dd	�Dd��De	�De��Df	�Df��Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj��Dk	�Dk��Dl	�Dl��Dm	�Dm��Dn	�Dn��Do	�Do��Dp	�Dp��Dq	�Dq��Dr	�Dr��Ds	�Ds��Dt	�Dt��Dx�gD�gD�K3D���D��3D��D�K3D�~gD���D�3D�NgD�^gD�� D��D�8 Dڤ�D���D��D�Q�D�{3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A�A�A�A�A�%A�A�  A�  A���A��A��TA��A��A��A���A���A�A�%A�
=A�
=A�JA�VA�bA�oA�bA��A��A��A�$�A�/A�9XA�;dA�?}A�O�A�hsA�p�AɅAɉ7Aɗ�Aɛ�Aə�Aɛ�Aɩ�Aɧ�A�z�A�G�A�x�A�A�S�A�S�A�K�A�|�A���A��
A�A��HA�=qA�O�A�%A���A��A�bNA��A���A��A�`BA���A�-A���A�K�A���A��A��wA��A�v�A�`BA�^5A�hsA��A�$�A��DA���A��A��9A�bNA�
=A�JA�ffA�7LA�JA�M�A��!A��+A�VA��DA�VA��A��A�-A��#A�hA}��A}�hA{x�Ax��Aw33An��Alr�Aj��Af��AcA`��A]&�AZbAT��AO�7AO7LAM��AH��AGhsAG/AF��AE��ACt�AB�A@��A?�wA>�`A=�FA<��A;��A;hsA;"�A9�-A8�uA6��A4�HA4I�A1��A09XA/hsA.^5A-p�A-�A,��A+��A*�A(��A'O�A&��A$�RA"��A!�A ��A z�A�mA��A�mA1A�yAv�A��A�/A�hA  AC�AJAx�A�TA��A�#A�/AȴAbNAJAp�A"�A�9A&�A
��A	K�A	7LA	�A	x�A	VA9XA�A��A��A�7A?}A�+A�A5?A n�A �9A9XAVA^5A�A"�A ��A =q@�~�@�j@�;d@�
=@�`B@���@�J@��`@��@�J@�%@��D@��@�v�@��/@띲@�^5@�-@�z�@旍@��H@��@ߕ�@��@���@� �@��m@܃@�+@�V@��@�Q�@��@�@ߥ�@���@ݡ�@��@���@۾w@���@�-@���@�Q�@��m@�\)@և+@�V@�^5@֟�@�@�S�@���@�=q@���@��;@�+@�=q@�z�@�\)@��H@���@�n�@��@ʏ\@�-@���@ȼj@�b@ȓu@���@�%@�V@ȴ9@�I�@�(�@�;d@�v�@š�@Ų-@�`B@�/@��@�j@þw@å�@�@°!@§�@���@�G�@��@��@�A�@���@�t�@��@���@��@��/@��u@��w@�v�@���@��^@�V@���@��u@��@���@��@�1'@�l�@�~�@�?}@�9X@�1@��F@�S�@��@��@�$�@���@��@��7@���@�x�@�r�@��F@�+@��!@�ff@�$�@��@�@�bN@�ƨ@���@��@�dZ@�+@��@�-@��-@���@�l�@��m@��;@�t�@��@�^5@�7L@��j@� �@�  @�1'@���@��m@��;@��P@��@���@�ff@���@�&�@��D@�1'@�b@��@���@�ƨ@��F@��@�\)@��H@�v�@�E�@��^@�V@��@�A�@��@��;@��P@�@�M�@�$�@�J@���@��7@�X@�/@�Ĝ@��@��m@�ƨ@��@�n�@�V@���@���@��^@��h@�`B@�`B@�p�@�`B@��@��9@�I�@��@�dZ@�33@���@�M�@�{@�{@��#@��7@�X@�G�@�X@�&�@��@��9@��u@�r�@���@��@�dZ@��@���@���@�7L@��j@��u@��@�I�@�  @��;@���@��w@���@��P@�S�@�+@�o@��H@��!@�~�@�v�@�ff@�n�@�E�@�5?@�$�@�$�@�$�@��@�J@��@���@�G�@�%@��@��/@��j@���@�z�@�bN@�Q�@�9X@��@��@��@�K�@��H@�ȴ@��+@�v�@�V@���@�hs@�S�@}�@t��@kt�@`��@Z~�@S33@M�-@G\)@?��@7�@0bN@*=q@&5?@!�7@�@�@�@bN@C�@�w11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A���A���A�A�A�A�A�%A�A�  A�  A���A��A��TA��A��A��A���A���A�A�%A�
=A�
=A�JA�VA�bA�oA�bA��A��A��A�$�A�/A�9XA�;dA�?}A�O�A�hsA�p�AɅAɉ7Aɗ�Aɛ�Aə�Aɛ�Aɩ�Aɧ�A�z�A�G�A�x�A�A�S�A�S�A�K�A�|�A���A��
A�A��HA�=qA�O�A�%A���A��A�bNA��A���A��A�`BA���A�-A���A�K�A���A��A��wA��A�v�A�`BA�^5A�hsA��A�$�A��DA���A��A��9A�bNA�
=A�JA�ffA�7LA�JA�M�A��!A��+A�VA��DA�VA��A��A�-A��#A�hA}��A}�hA{x�Ax��Aw33An��Alr�Aj��Af��AcA`��A]&�AZbAT��AO�7AO7LAM��AH��AGhsAG/AF��AE��ACt�AB�A@��A?�wA>�`A=�FA<��A;��A;hsA;"�A9�-A8�uA6��A4�HA4I�A1��A09XA/hsA.^5A-p�A-�A,��A+��A*�A(��A'O�A&��A$�RA"��A!�A ��A z�A�mA��A�mA1A�yAv�A��A�/A�hA  AC�AJAx�A�TA��A�#A�/AȴAbNAJAp�A"�A�9A&�A
��A	K�A	7LA	�A	x�A	VA9XA�A��A��A�7A?}A�+A�A5?A n�A �9A9XAVA^5A�A"�A ��A =q@�~�@�j@�;d@�
=@�`B@���@�J@��`@��@�J@�%@��D@��@�v�@��/@띲@�^5@�-@�z�@旍@��H@��@ߕ�@��@���@� �@��m@܃@�+@�V@��@�Q�@��@�@ߥ�@���@ݡ�@��@���@۾w@���@�-@���@�Q�@��m@�\)@և+@�V@�^5@֟�@�@�S�@���@�=q@���@��;@�+@�=q@�z�@�\)@��H@���@�n�@��@ʏ\@�-@���@ȼj@�b@ȓu@���@�%@�V@ȴ9@�I�@�(�@�;d@�v�@š�@Ų-@�`B@�/@��@�j@þw@å�@�@°!@§�@���@�G�@��@��@�A�@���@�t�@��@���@��@��/@��u@��w@�v�@���@��^@�V@���@��u@��@���@��@�1'@�l�@�~�@�?}@�9X@�1@��F@�S�@��@��@�$�@���@��@��7@���@�x�@�r�@��F@�+@��!@�ff@�$�@��@�@�bN@�ƨ@���@��@�dZ@�+@��@�-@��-@���@�l�@��m@��;@�t�@��@�^5@�7L@��j@� �@�  @�1'@���@��m@��;@��P@��@���@�ff@���@�&�@��D@�1'@�b@��@���@�ƨ@��F@��@�\)@��H@�v�@�E�@��^@�V@��@�A�@��@��;@��P@�@�M�@�$�@�J@���@��7@�X@�/@�Ĝ@��@��m@�ƨ@��@�n�@�V@���@���@��^@��h@�`B@�`B@�p�@�`B@��@��9@�I�@��@�dZ@�33@���@�M�@�{@�{@��#@��7@�X@�G�@�X@�&�@��@��9@��u@�r�@���@��@�dZ@��@���@���@�7L@��j@��u@��@�I�@�  @��;@���@��w@���@��P@�S�@�+@�o@��H@��!@�~�@�v�@�ff@�n�@�E�@�5?@�$�@�$�@�$�@��@�J@��@���@�G�@�%@��@��/@��j@���@�z�@�bN@�Q�@�9X@��@��@��@�K�@��H@�ȴ@��+@�v�@�V@���G�O�@�S�@}�@t��@kt�@`��@Z~�@S33@M�-@G\)@?��@7�@0bN@*=q@&5?@!�7@�@�@�@bN@C�@�w11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�
B�
B�
B�
B�B�
B�B�#B�#B�)B�5B�;B�BB�HB�HB�NB�NB�NB�TB�TB�`B�mB�mB�yB�B�B�B�B��B��B��B	B	B	%B	1B		7B	
=B	VB	hB	�B	�B	2-B	N�B	�+B
DB
ZB
|�B
�{B
�B
��B
��B�B.BN�BaHB{�B�{Br�BbNBB�B+B%�B,B2-B?}B?}BW
B
��BK�BdZBaHBe`Bl�BbNBP�BJB
�B
�)B
�9B
�oB
~�B
t�B
q�B
u�B
�VB
z�B
ffB
dZB
S�B
I�B
=qB
JB	�B	�ZB	��B	�qB	�FB	�-B	��B	��B	�1B	bNB	Q�B	H�B	5?B	&�B	�B	DB��B�yB��B��BȴB��B��B��B��BȴB�}B�RB�'B�B�B��B��B�B�B�B�'B�!B�9B�9B�'B�B�B�B��B��B��B��B��B��B��B�\B�DB�+B�+B�VB�oB��B�uB�PB�=B�=B�DB�DB�JB�{B�'B�qB�^B�3B�B��B��B�{B}�Bx�Bz�B{�B}�B�B� Bt�Bs�Br�B�%B��B��B��B��B�B�B�B��B��B��B�3B��B��B��B��BÖBĜBŢBĜBB�qB�FB�3B�?B�RB�}B�dB�?B�?B�?B�FB�LB�LB�FB�9B�!B�B�B��B��B��B��B��B�uB�VB�JB�PB�VB��B�B�qBĜB��B��BɺBɺB��B��B��B�;B�;B�/B�#B�)B�;B�BB�;B�;B�BB�HB�TB�sB�B�B��B	  B	%B	1B	
=B	JB	JB	JB	DB		7B	%B��B��B	B	B	+B	bB	{B	�B	�B	�B	"�B	)�B	(�B	(�B	)�B	,B	.B	/B	1'B	2-B	2-B	1'B	2-B	49B	6FB	<jB	>wB	?}B	?}B	A�B	C�B	C�B	D�B	G�B	I�B	K�B	K�B	M�B	Q�B	T�B	T�B	T�B	XB	YB	ZB	[#B	aHB	hsB	k�B	l�B	l�B	m�B	m�B	n�B	p�B	q�B	p�B	u�B	w�B	y�B	z�B	z�B	}�B	�B	�B	�B	�+B	�7B	�=B	�DB	�DB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�-B	�-B	�3B	�?B	�LB	�RB	�RB	�XB	�XB	�XB	�^B	�XB	�dB	�jB	�qB	�wB	�}B	B	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�/B	�/B	�/B	�/B	�;B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�NB	�TB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B

=B
uB
�B
&�B
,B
33B
9XB
=qB
C�B
J�B
R�B
YB
\)B
aHB
e`B
hsB
l�B
q�B
v�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�!B�-B�,B�4B�=B�GB�MB�OB�QB�YB�XB�VB�^B�\B�kB�yB�yB�B�B�B�B�B��B��B��B	B	B	,B	:B		?B	
CB	_B	nB	�B	�B	21B	N�B	�+B
?B
ZB
|�B
�sB
�B
��B
��B�B.BN�Ba>B{�B�oBr�BbABB�B*�B%�B+�B2!B?rB?tBV�B
��BK�BdKBa:BeTBl�Bb?BP�B@B
�B
�B
�.B
�dB
~�B
t�B
q�B
u�B
�JB
z�B
f]B
dNB
S�B
I�B
=jB
CB	�B	�UB	��B	�kB	�AB	�)B	��B	��B	�.B	bLB	Q�B	H�B	5>B	&�B	�B	DB��B�zB��B��BȸB��B��B��B��BȹB��B�UB�+B�B�B��B�B�B�B�B�)B�#B�=B�;B�+B�B�B�B� B��B��B��B��B��B��B�_B�LB�2B�3B�ZB�sB��B�xB�TB�AB�BB�HB�KB�OB�~B�&B�rB�cB�4B�B��B��B�|B}�Bx�Bz�B{�B}�B�B�Bt�Bs�Br�B�'B��B��B��B��B�B�
B�B��B��B��B�4B��B��B��B��BÖBĞBŠBĞBB�sB�HB�4B�AB�SB�B�fB�@B�=B�=B�GB�KB�LB�FB�9B�"B�B�B��B��B��B��B��B�vB�XB�JB�QB�VB��B�B�pBĜB��B��BɻBɸB��B��B��B�;B�7B�+B�"B�'B�;B�?B�8B�8B�AB�EB�SB�qB�B�B��B��B	!B	-B	
6B	EB	DB	EB	AB		4B	!B��B��B	B	B	'B	`B	yB	�B	�B	�B	"�B	)�B	(�B	(�B	)�B	, B	.B	/B	1"B	2*B	2'B	1 B	2&B	43B	6>B	<cB	>pB	?wB	?xB	A�B	C�B	C�B	D�B	G�B	I�B	K�B	K�B	M�B	Q�B	T�B	T�B	T�B	XB	YB	ZB	[B	a@B	hlB	k�B	l�B	l�B	m�B	m�B	n�B	p�B	q�B	p�B	u�B	w�B	y�B	z�B	z�B	}�B	��B	�	B	�B	�!B	�-B	�4B	�<B	�:B	�^B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�	B	�B	�"B	�"B	�!B	�(B	�3B	�>B	�EB	�HB	�LB	�MB	�MB	�QB	�MB	�YB	�_B	�fB	�mB	�rB	B	ÌB	ĐB	ĒB	ēB	ŘB	ƜB	ɯB	ʷB	˼B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�$B	�"B	�.B	�-B	�.B	�-B	�5B	�7B	�=B	�;B	�@B	�HB	�SB	�WB	�bB	�fB	�sB	�pB	�tB	�qB	�yB	�yB	�zB	�yB	�qB	�rB	�wB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
B

/B
hB
�B
&�B
+�B
3&B
9KB
=cB
C�B
J�B
R�B
YB
\B
a7B
eQB
hdB
l{B
q�B
v�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.15 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436452016080714364520160807143645  AO  ARCAADJP                                                                    20160324194910    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160324194910  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160324194910  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143645  IP                  G�O�G�O�G�O�                