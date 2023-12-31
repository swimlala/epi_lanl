CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-11-18T10:16:18Z AOML 3.0 creation; 2016-06-01T00:08:27Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151118101618  20160531170827  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_133                   2C  D   APEX                            5374                            041511                          846 @����`1   @���M^�@;E�����c�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�  @�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�3D�3D�C3D���D��3D���D�L�D���D���D��D�@ D���DǼ�D���D�<�Dڙ�D��3D�fD�C3D�ffD�ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@���AffA"ffABffA`��A�33A�33A�33A�33A�33A�33A�33A�33B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�� B�� B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�� B�L�B�L�B�L�B�L�B�L�B�L�B�L�C &fC&fC&fC&fC&fC
&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC &fC"&fC$&fC&&fC(&fC*&fC,&fC.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd&fCf&fCh&fCj&fCl&fCn&fCp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�fC�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D ��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D		�D	��D
	�D
��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D 	�D ��D!	�D!��D"	�D"��D#	�D#��D$	�D$��D%	�D%��D&	�D&��D'	�D'��D(	�D(��D)	�D)��D*	�D*��D+	�D+��D,	�D,��D-	�D-��D.	�D.��D/	�D/��D0	�D0��D1	�D1��D2	�D2��D3	�D3��D4	�D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8��D9	�D9��D:	�D:��D;	�D;��D<	�D<��D=	�D=��D>	�D>��D?	�D?��D@	�D@��DA	�DA��DB	�DB��DC	�DC��DD	�DD��DE	�DE��DF	�DF��DG	�DG��DH	�DH��DI	�DI��DJ	�DJ��DK	�DK��DL	�DL��DM	�DM��DN	�DN��DO	�DO��DP	�DP��DQ	�DQ��DR	�DR��DS	�DS��DT	�DT��DU	�DU��DV	�DV��DW	�DW��DX DX��DY	�DY��DZ	�DZ��D[	�D[��D\	�D\��D]	�D]��D^	�D^��D_	�D_��D`	�D`��Da	�Da��Db	�Db��Dc	�Dc��Dd	�Dd��De	�De��Df	�Df��Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj��Dk	�Dk��Dl	�Dl��Dm	�Dm��Dn	�Dn��Do	�Do��Dp	�Dp��Dq	�Dq��Dr	�Dr��Ds	�Ds��Dt	�Dt�4Dy��D� D�H D��gD�� D��gD�Q�D���D���D�gD�D�D��gD���D��gD�A�DڞgD�� D�3D�H D�k3D�k311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A��A��A��A��A��A��A��A���A���A��HA���A��
A���A���A���A���A�ƨA���A���A���A���A���A���A�ĜA�ƨA�ĜA�ĜA�ƨA�ĜA�ĜA�A���A���AɾwAɑhA�E�A�n�A���A�v�A��^A���A�dZA���A��A���A��TA��RA��A�n�A��A�%A��7A�=qA�XA�{A�-A��PA�33A��TA�ZA��HA�1A�VA�-A�1'A���A�^5A�1'A��^A�K�A���A�XA�VA�~�A��A��uA�x�A��A�z�A�%A�M�A�v�A��A���A���A��hA�A���A�1'A��\A�r�A���A��RA�~�A��A�I�A�;dA�(�A��A�|�A��yA��uA�S�A�+A�{A���A���A� �A�ƨA�`BA�
=A��A�+A�n�A��A|�Az�9Ay��Ax�HAxJAv  Au\)Ar~�Ap��Ao�TAnv�Am7LAl��Ak��AhȴAd��A^bNA\�A\n�A[�#A[S�A["�AZ�RAZ1'AY�#AY�AX�AVffAS/AQ`BAPE�AO�AM��AM
=AL�+AM�AL�uAK`BAKoAJ�AJĜAJ$�AI��AHbAFȴAFv�AFJAE�-AEp�AE;dAD��AD��ADbNACƨAB�AAl�A@ZA?/A>��A>jA>{A=�FA<�jA:�A9"�A8ZA7ƨA7��A7x�A7\)A7/A6��A6ZA4bA3K�A2ȴA1�mA1`BA0�HA0=qA-�;A+�#A+%A*(�A)+A(Q�A'A$1'A"�A!�wA!hsA!7LA ��A n�A M�A �A�A��A�A1Al�A^5A�A{AVAbNAA��A�A�+AS�AZAp�AE�AoA(�A��A�A	��A��A��A�AS�A`BAG�A�/A�A�hAG�A ��@�+@�r�@�+@�v�@��@�`B@��@���@�33@��T@�A�@�\@�M�@�hs@�(�@�F@�P@�dZ@�o@��`@���@�@���@�  @���@䛦@�@�J@��;@��@�o@�
=@��y@ް!@�v�@�ff@�M�@�$�@�J@ݺ^@�p�@��@ۮ@��@���@�1@׍P@�o@��y@�~�@�I�@Гu@�ff@���@̓u@˕�@��@�@�O�@�9X@�t�@ź^@�1'@öF@�K�@��@§�@��@�/@��@���@��@�J@�V@��@���@�r�@�9X@��F@�;d@��h@��m@��@�@��T@��@��@� �@�t�@�@�&�@���@�Q�@�A�@�(�@���@��F@�t�@�;d@�"�@��@���@�V@��@�o@��R@��+@�5?@��-@��@�ȴ@��@���@��@��@�K�@�33@��@�@��y@���@�ȴ@���@�^5@�-@�@���@��@�@���@��w@��@��R@�-@���@���@��@�`B@�X@�/@���@�I�@��@�n�@��7@�V@��j@��m@�dZ@��@�V@��@���@���@��j@�Z@��@���@�E�@��T@���@��/@�r�@��w@���@�t�@�\)@�33@��@���@�v�@�@�@��7@�X@�?}@�7L@�7L@�&�@��/@�j@�1@��m@��
@�ƨ@���@��@��@��@�t�@�dZ@�t�@��w@��w@��@���@���@�|�@�S�@�K�@�C�@�K�@�;d@�"�@�o@�
=@�@�E�@�@�`B@��/@��@�9X@�9X@�(�@�1'@�(�@�1@��;@��F@��P@�|�@�K�@��@��R@��!@��H@���@�^5@�J@��@��T@��T@�@�X@�&�@��@�I�@��w@��@��@|�j@s33@k�m@eV@\z�@SC�@K��@C�F@:=q@4��@1�@-?}@)%@#@ȴ@b@Z@\)@Z@�P11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A��A��A��A��A��A��A��A���A���A��HA���A��
A���A���A���A���A�ƨA���A���A���A���A���A���A�ĜA�ƨA�ĜA�ĜA�ƨA�ĜA�ĜA�A���A���AɾwAɑhA�E�A�n�A���A�v�A��^A���A�dZA���A��A���A��TA��RA��A�n�A��A�%A��7A�=qA�XA�{A�-A��PA�33A��TA�ZA��HA�1A�VA�-A�1'A���A�^5A�1'A��^A�K�A���A�XA�VA�~�A��A��uA�x�A��A�z�A�%A�M�A�v�A��A���A���A��hA�A���A�1'A��\A�r�A���A��RA�~�A��A�I�A�;dA�(�A��A�|�A��yA��uA�S�A�+A�{A���A���A� �A�ƨA�`BA�
=A��A�+A�n�A��A|�Az�9Ay��Ax�HAxJAv  Au\)Ar~�Ap��Ao�TAnv�Am7LAl��Ak��AhȴAd��A^bNA\�A\n�A[�#A[S�A["�AZ�RAZ1'AY�#AY�AX�AVffAS/AQ`BAPE�AO�AM��AM
=AL�+AM�AL�uAK`BAKoAJ�AJĜAJ$�AI��AHbAFȴAFv�AFJAE�-AEp�AE;dAD��AD��ADbNACƨAB�AAl�A@ZA?/A>��A>jA>{A=�FA<�jA:�A9"�A8ZA7ƨA7��A7x�A7\)A7/A6��A6ZA4bA3K�A2ȴA1�mA1`BA0�HA0=qA-�;A+�#A+%A*(�A)+A(Q�A'A$1'A"�A!�wA!hsA!7LA ��A n�A M�A �A�A��A�A1Al�A^5A�A{AVAbNAA��A�A�+AS�AZAp�AE�AoA(�A��A�A	��A��A��A�AS�A`BAG�A�/A�A�hAG�A ��@�+@�r�@�+@�v�@��@�`B@��@���@�33@��T@�A�@�\@�M�@�hs@�(�@�F@�P@�dZ@�o@��`@���@�@���@�  @���@䛦@�@�J@��;@��@�o@�
=@��y@ް!@�v�@�ff@�M�@�$�@�J@ݺ^@�p�@��@ۮ@��@���@�1@׍P@�o@��y@�~�@�I�@Гu@�ff@���@̓u@˕�@��@�@�O�@�9X@�t�@ź^@�1'@öF@�K�@��@§�@��@�/@��@���@��@�J@�V@��@���@�r�@�9X@��F@�;d@��h@��m@��@�@��T@��@��@� �@�t�@�@�&�@���@�Q�@�A�@�(�@���@��F@�t�@�;d@�"�@��@���@�V@��@�o@��R@��+@�5?@��-@��@�ȴ@��@���@��@��@�K�@�33@��@�@��y@���@�ȴ@���@�^5@�-@�@���@��@�@���@��w@��@��R@�-@���@���@��@�`B@�X@�/@���@�I�@��@�n�@��7@�V@��j@��m@�dZ@��@�V@��@���@���@��j@�Z@��@���@�E�@��T@���@��/@�r�@��w@���@�t�@�\)@�33@��@���@�v�@�@�@��7@�X@�?}@�7L@�7L@�&�@��/@�j@�1@��m@��
@�ƨ@���@��@��@��@�t�@�dZ@�t�@��w@��w@��@���@���@�|�@�S�@�K�@�C�@�K�@�;d@�"�@�o@�
=@�@�E�@�@�`B@��/@��@�9X@�9X@�(�@�1'@�(�@�1@��;@��F@��P@�|�@�K�@��@��R@��!@��H@���@�^5@�J@��@��T@��T@�@�X@�&�@��@�I�@��w@��@��@|�j@s33@k�m@eV@\z�@SC�@K��@C�F@:=q@4��@1�@-?}@)%@#@ȴ@b@Z@\)@Z@�P11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB{B{B{B{B{B{B{B{B{B{B{B{BuBoBoBoBoBoBoBhBoBoBhBhBoBhBhBhBhBhBhBhBhBhBbBbB\BJBB
=BVBbB�B&�B33B49B9XB;dB;dB8RB33B+B �B�B �B!�B"�B�BJBDBJBPB
=B%B��B��B�B�NB�)B�BɺBÖB�qB�LB�3B�B��B�VBk�BZBF�B>wB5?B�BhB��B�BǮB�3B��B��B�\B�+B�Bz�BgmBS�BG�BB�BA�B?}B<jB7LB/B)�B&�B$�B#�B!�B�B�BhBDB%B
��B
��B
�mB
��B
��B
�oB
�7B
�B
x�B
iyB
aHB
K�B
:^B
49B
&�B
�B
uB
%B	�mB	ÖB	�uB	�%B	�B	}�B	y�B	w�B	t�B	p�B	m�B	iyB	`BB	O�B	=qB	7LB	0!B	)�B	#�B	$�B	0!B	Q�B	XB	P�B	O�B	O�B	P�B	L�B	H�B	B�B	>wB	<jB	:^B	9XB	7LB	6FB	49B	2-B	/B	+B	#�B	�B	�B	{B	oB	hB	VB	DB	B��B�B�B�B�B�B�B�yB�mB�NB�#B�B��B��B��B��BȴB��B�^B�FB�-B�B��B��B��B�hB�\B�VB�VB�JB�DB�=B�7B�1B�%B�B�B~�Bz�Bw�Bs�Bo�BffBcTBcTBbNB`BB]/BZBXBT�BQ�BP�BO�BL�BI�BF�BE�BD�BA�B=qB;dB9XB9XB8RB7LB49B2-B1'B0!B0!B/B.B-B,B+B)�B'�B'�B&�B%�B%�B%�B%�B$�B"�B!�B!�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B!�B"�B!�B#�B&�B'�B(�B(�B(�B(�B+B)�B,B-B.B1'B1'B1'B1'B1'B2-B2-B49B7LB;dB>wB>wB>wB>wB@�BB�BG�BK�BM�BO�BO�BO�BP�BQ�BR�BS�BS�BT�BT�BT�B^5BbNBcTBdZBffBiyBt�B{�B|�B�B�+B�DB�JB�PB�VB�\B�bB�bB�bB�hB�oB�{B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�9B�XB�}BĜBǮBɺB��B�B�B�5B�BB�BB�BB�ZB�fB�fB�yB�B�B�B�B�B��B��B��B��B��B��B��B��B	B	+B		7B	DB	JB	JB	JB	JB	\B	{B	�B	�B	�B	�B	 �B	!�B	$�B	'�B	(�B	(�B	+B	.B	1'B	33B	5?B	5?B	7LB	=qB	>wB	?}B	A�B	C�B	E�B	G�B	I�B	J�B	P�B	R�B	W
B	XB	[#B	_;B	`BB	bNB	bNB	cTB	dZB	dZB	e`B	gmB	hsB	gmB	hsB	jB	l�B	q�B	r�B	t�B	v�B	x�B	y�B	y�B	y�B	{�B	~�B	�B	�B	�%B	�1B	�bB	��B	ĜB	�B	�B
  B
JB
�B
�B
#�B
/B
7LB
=qB
D�B
L�B
Q�B
ZB
aHB
e`B
iyB
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BsBsBsBsBsBqBqBsBqBqBqBsBlBeBeBgBeBeBeBZBeBeB`B`BeB`B]B]B]B]B]B`B]B\BWBYBOB=BB
0BNBYB�B&�B3&B4/B9MB;ZB;XB8HB3'B*�B �BqB �B!�B"�BxB>B7B>BDB
2BB��B��B�B�BB�B��BɰBÆB�dB�?B�$B�B��B�HBkuBZBF�B>hB5/B�B[B��B��BǟB�&B��B��B�JB�B�	Bz�BgbBS�BG�BBBA{B?nB<[B7>B/B)�B&�B$�B#�B!�B�BxBZB8BB
��B
��B
�aB
˽B
��B
�dB
�,B
��B
x�B
ioB
a=B
K�B
:SB
41B
&�B
�B
mB
B	�fB	ÐB	�oB	� B	�	B	}�B	y�B	w�B	t�B	p�B	m�B	iuB	`@B	O�B	=rB	7KB	0!B	)�B	#�B	$�B	0 B	Q�B	XB	P�B	O�B	O�B	P�B	L�B	H�B	B�B	>sB	<gB	:[B	9VB	7JB	6EB	48B	2*B	/B	+B	#�B	�B	�B	|B	pB	gB	XB	EB	B��B�B�B�B�B�B�B�zB�pB�QB�&B�B��B��B��B��BȷB��B�`B�HB�2B�B��B��B��B�mB�aB�YB�YB�PB�IB�BB�:B�4B�+B�B�BBz�Bw�Bs�Bo�BfkBcZBcXBbUB`GB]6BZ$BXBUBQ�BP�BO�BL�BI�BF�BE�BD�BA�B=_B;lB9`B9aB8XB7QB4@B23B1-B0*B0(B/$B.B-B,B+B*B'�B'�B&�B%�B%�B%�B%�B$�B"�B!�B!�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B!�B"�B!�B#�B&�B'�B(�B(�B(�B(�B+B*B,B-B.B1.B1/B1-B1.B1.B23B21B4?B7OB;lB>|B>{B>xB>zB@�BB�BG�BK�BM�BO�BO�BO�BP�BQ�BR�BS�BS�BU BUBU B^7BbQBcWBd[BfhBi{Bt�B{�B|�B�B�-B�EB�IB�NB�WB�[B�cB�dB�dB�iB�qB�{B�B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�$B�6B�TB�yBĚBǪBɻB��B�B�B�0B�>B�=B�>B�WB�cB�bB�vB�{B�B�B�B�B��B��B��B��B��B��B��B��B	B	$B		0B	=B	BB	EB	EB	EB	UB	vB	�B	�B	�B	�B	 �B	!�B	$�B	'�B	(�B	(�B	*�B	.B	1B	3,B	58B	58B	7DB	=iB	>nB	?sB	A�B	C�B	E�B	G�B	I�B	J�B	P�B	R�B	WB	X	B	[B	_2B	`8B	bCB	bDB	cMB	dRB	dQB	eVB	gcB	hiB	gcB	hjB	jwB	l�B	q�B	r�B	t�B	v�B	x�B	y�B	y�B	y�B	{�B	~�B	��B	�	B	�B	�(B	�XB	��B	ĒB	�B	�xB	��B
=B
�B
�B
#�B
/B
7?B
=dB
D�B
L�B
Q�B
ZB
a9B
eQB
ikB
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.15 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708272016053117082720160531170827  AO  ARCAADJP                                                                    20151118101618    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151118101618  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151118101618  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170827  IP                  G�O�G�O�G�O�                