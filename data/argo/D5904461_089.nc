CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-12-15T20:19:31Z AOML 3.0 creation; 2016-08-07T21:36:41Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20151215201931  20160807143642  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               YA   AO  5286_8897_089                   2C  D   APEX                            6531                            072314                          846 @׆Z���1   @׆\)���@3U�$�/�c6I�^51   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    YA   B   B   @���@�  @���A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� DlfDl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtL�DyS3D�3D�S3D�� D���D�fD�6fD�|�D��3D���D�C3D��fD���D���D�6fDڌ�D�� D�� D�33D� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�fg@���A ��A"ffABffAbffA�33A�33A�ffA�33A�33A�33A�33A�33B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�� B�� B��B�L�B�L�B��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C &fC&fC&fC&fC&fC
&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC &fC"&fC$&fC&&fC(&fC*&fC,&fC.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX@ CZ&fC\&fC^&fC`&fCb&fCd&fCf&fCh&fCj&fCl&fCn@ Cp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D ��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D		�D	��D
	�D
��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D 	�D ��D!	�D!��D"	�D"��D#	�D#��D$	�D$��D%	�D%��D&	�D&��D'	�D'��D(	�D(��D)	�D)��D* D*��D+	�D+��D,	�D,��D-	�D-��D.	�D.��D/	�D/��D0	�D0��D1	�D1��D2	�D2��D3	�D3��D4	�D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8��D9	�D9��D:	�D:��D;	�D;��D<	�D<��D=	�D=��D>	�D>��D?	�D?��D@	�D@��DA	�DA��DB	�DB��DC	�DC��DD	�DD��DE	�DE��DF	�DF��DG	�DG��DH	�DH��DI	�DI��DJ	�DJ��DK	�DK��DL	�DL��DM	�DM��DN	�DN��DO	�DO��DP	�DP��DQ	�DQ��DR	�DR��DS	�DS��DT	�DT��DU	�DU��DV	�DV��DW	�DW��DX	�DX��DY	�DY��DZ	�DZ��D[	�D[��D\	�D\��D]	�D]��D^	�D^��D_	�D_��D`	�D`��Da	�Da��Db	�Db��Dc	�Dc��Dd	�Dd��De	�De��Df	�Df��Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj��Dk	�Dk��Dl Dl��Dm	�Dm��Dn	�Dn��Do	�Do��Dp	�Dp��Dq	�Dq��Dr	�Dr��Ds	�Ds��Dt	�DtVgDy\�D� D�X D���D�њD�3D�;3D���D�� D��D�H D��3D�њD��gD�;3Dڑ�D���D���D�8 D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�9XA�9XA�9XA�?}A�?}A�A�A�C�A�E�A�G�A�O�A�Q�A�O�A�Q�A�Q�A�S�A�Q�A�VA�S�A�VA�VA�VA�XA�XA�`BA�dZA�ffA�hsA�hsA�n�A�v�AЅA��#A�1A��A� �A�+A�&�A�/A�(�A��AЅAϩ�A���A�|�A�Ȧ+A�JAǝ�A�~�A��A���A��A���A��\A�1'A�ĜA�bNA���A��#A��/A�{A�"�A�5?A��-A�&�A��PA��HA�x�A���A�G�A��A��!A��!A��A�1'A�^5A��A�K�A���A��^A�JA�1A��#A�1'A��;A�ȴA�1A�~�A��uA���A�x�A���A�K�A�-A���A��mA�bA��A��A�E�A�`BA�\)A���A�bA���A��`A�C�A�A�AhsA?}A~�HA~��A}x�A|  Azv�Ay�At��ArA�AoƨAn$�AmdZAlz�Aj��Ah��Af��Ad5?AbȴAax�A_+AZ=qAW��AV^5AT�!AS/AP=qAOXAM?}AL  AKt�AI�^AEƨAB9XA>9XA:�9A7�PA4ĜA3?}A1dZA/`BA-G�A,��A,^5A+�A*jA)|�A(ȴA'A'�7A'�PA'dZA%�FA"�A"(�A �`A�Al�A�AA�Ax�A��AI�A��A�A�RA�^AVAx�AbA��A=qAƨAp�A�HA�yA�#A  A��A"�AdZA1'A
=qA	x�AJA�A��AZAffAoA��AA �+@��@��@�33@�@�M�@�x�@� �@���@���@�(�@���@��@�{@�r�@�{@�O�@��9@�b@��H@�x�@�G�@�V@���@��T@�n�@��H@ى7@ם�@�n�@�?}@�A�@�\)@�v�@���@�E�@ӕ�@�O�@�dZ@�+@�^5@֗�@֏\@�hs@��;@�+@�$�@�X@�O�@�t�@���@���@ѩ�@��@мj@�z�@θR@�S�@ȃ@�ȴ@���@Ĭ@�t�@�/@�+@�o@�@��@��H@��R@�5?@��-@�bN@��@��+@��9@�  @��;@�ƨ@��@�33@�v�@���@�`B@���@�-@�7L@��@��R@�~�@���@�Ĝ@�bN@��@�C�@��R@�-@��R@�^5@��@��T@��@��@��u@��@��9@���@�r�@�o@��!@���@�X@�&�@��-@�X@�9X@��;@�V@�G�@��`@��u@�ƨ@�dZ@��h@�v�@��
@�v�@�V@�ƨ@��;@��;@�I�@��D@�@��@�$�@���@�Ĝ@��9@��D@���@�G�@��@��j@�Ĝ@��P@�^5@��-@��@��j@�Q�@�A�@�9X@�(�@��
@��R@��+@��@�$�@�-@��^@���@��j@�G�@�X@���@���@�V@��`@��@�Q�@�9X@�9X@�A�@�I�@��D@�bN@�b@��;@�dZ@���@�v�@�@�@��@�J@��-@�%@���@���@�Ĝ@��j@��@�1@��w@�ƨ@�l�@��!@�n�@�ff@���@��@��y@��R@�~�@�ff@�$�@�J@��@�J@�l�@��D@�?}@�@��@�j@�l�@��y@��+@�M�@�@���@�G�@�%@���@��j@���@��@�j@�1@�l�@�\)@�K�@�+@�o@��@��H@���@�ȴ@��!@�V@�5?@�@���@��@�O�@�&�@�r�@�I�@�bN@��@�bN@�I�@�I�@�I�@�Q�@�Z@�Z@� �@�|�@�@�v�@�~�@�@��-@�`B@��@���@���@�1'@�b@���@��@��@��@�|�@�33@��H@��H@��@�
=@�
=@��y@�M�@�5?@���@�G�@��j@���@|�D@p�9@hb@^��@W�;@O�@F@=�T@8�9@2�@-�@&V@!hs@z�@A�@ƨ@  @9X@Q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111A�9XA�9XA�9XA�?}A�?}A�A�A�C�A�E�A�G�A�O�A�Q�A�O�A�Q�A�Q�A�S�A�Q�A�VA�S�A�VA�VA�VA�XA�XA�`BA�dZA�ffA�hsA�hsA�n�A�v�AЅA��#A�1A��A� �A�+A�&�A�/A�(�A��AЅAϩ�A���A�|�A�Ȧ+A�JAǝ�A�~�A��A���A��A���A��\A�1'A�ĜA�bNA���A��#A��/A�{A�"�A�5?A��-A�&�A��PA��HA�x�A���A�G�A��A��!A��!A��A�1'A�^5A��A�K�A���A��^A�JA�1A��#A�1'A��;A�ȴA�1A�~�A��uA���A�x�A���A�K�A�-A���A��mA�bA��A��A�E�A�`BA�\)A���A�bA���A��`A�C�A�A�AhsA?}A~�HA~��A}x�A|  Azv�Ay�At��ArA�AoƨAn$�AmdZAlz�Aj��Ah��Af��Ad5?AbȴAax�A_+AZ=qAW��AV^5AT�!AS/AP=qAOXAM?}AL  AKt�AI�^AEƨAB9XA>9XA:�9A7�PA4ĜA3?}A1dZA/`BA-G�A,��A,^5A+�A*jA)|�A(ȴA'A'�7A'�PA'dZA%�FA"�A"(�A �`A�Al�A�AA�Ax�A��AI�A��A�A�RA�^AVAx�AbA��A=qAƨAp�A�HA�yA�#A  A��A"�AdZA1'A
=qA	x�AJA�A��AZAffAoA��AA �+@��@��@�33@�@�M�@�x�@� �@���@���@�(�@���@��@�{@�r�@�{@�O�@��9@�b@��H@�x�@�G�@�V@���@��T@�n�@��H@ى7@ם�@�n�@�?}@�A�@�\)@�v�@���@�E�@ӕ�@�O�@�dZ@�+@�^5@֗�@֏\@�hs@��;@�+@�$�@�X@�O�@�t�@���@���@ѩ�@��@мj@�z�@θR@�S�@ȃ@�ȴ@���@Ĭ@�t�@�/@�+@�o@�@��@��H@��R@�5?@��-@�bN@��@��+@��9@�  @��;@�ƨ@��@�33@�v�@���@�`B@���@�-@�7L@��@��R@�~�@���@�Ĝ@�bN@��@�C�@��R@�-@��R@�^5@��@��T@��@��@��u@��@��9@���@�r�@�o@��!@���@�X@�&�@��-@�X@�9X@��;@�V@�G�@��`@��u@�ƨ@�dZ@��h@�v�@��
@�v�@�V@�ƨ@��;@��;@�I�@��D@�@��@�$�@���@�Ĝ@��9@��D@���@�G�@��@��j@�Ĝ@��P@�^5@��-@��@��j@�Q�@�A�@�9X@�(�@��
@��R@��+@��@�$�@�-@��^@���@��j@�G�@�X@���@���@�V@��`@��@�Q�@�9X@�9X@�A�@�I�@��D@�bN@�b@��;@�dZ@���@�v�@�@�@��@�J@��-@�%@���@���@�Ĝ@��j@��@�1@��w@�ƨ@�l�@��!@�n�@�ff@���@��@��y@��R@�~�@�ff@�$�@�J@��@�J@�l�@��D@�?}@�@��@�j@�l�@��y@��+@�M�@�@���@�G�@�%@���@��j@���@��@�j@�1@�l�@�\)@�K�@�+@�o@��@��H@���@�ȴ@��!@�V@�5?@�@���@��@�O�@�&�@�r�@�I�@�bN@��@�bN@�I�@�I�@�I�@�Q�@�Z@�Z@� �@�|�@�@�v�@�~�@�@��-@�`B@��@���@���@�1'@�b@���@��@��@��@�|�@�33@��H@��H@��@�
=@�
=@��y@�M�@�5?@���@�G�G�O�@���@|�D@p�9@hb@^��@W�;@O�@F@=�T@8�9@2�@-�@&V@!hs@z�@A�@ƨ@  @9X@Q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
R�B
R�B
S�B
S�B
S�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
S�B
T�B
ZB
hsB
u�B
��B
�XB
ǮB
��B
�BBoB"�B49BD�BjB�B��B��B�}B�#B��B  BBVBt�Bw�B{�B�B�1B��B�B�XBĜBȴBĜB�dBĜBƨBBĜB�wB�FB�FB�LB�FB�9B��B�DB�Be`BW
BH�B0!B��BȴB�XB�-B��B�\Bs�BT�BC�B:^B9XB0!B(�B'�B#�BhB+B
��B+B
��B
�HB
��B
ÖB
�B
�VB
q�B
Q�B
(�B
�B
�B
�B
uB

=B	��B	�B	�HB	ȴB	�9B	��B	��B	�bB	�DB	}�B	o�B	dZB	T�B	J�B	@�B	0!B	�B	
=B	B��B��B�B�fB�;B�#B�B��BƨB�wB�RB�!B��B��B��B��B�{B�bB�VB�DB�1B�+B�=B�=B�JB�hB��B��B��B�oB�JB�PB�1B}�Bx�By�Bw�Bw�B�B�PB�\B�VB�JB�1B�+B�B�B�B�B�+B�B�+B��B�B�wBǮB�^B�!B��B��B�uB�DB�7B�+B�JB�BjBaHBcTBcTBbNBaHB_;B\)BYBW
BR�B^5Bk�Bl�BjBiyBhsBgmBffBffBcTB^5BQ�BC�B=qB@�B?}B=qB9XB8RB;dB<jB?}BH�BO�BR�BVB]/Bk�Bx�B�7B�bB��B��B��B��B��B��B��B��B��B�RB�wB�^B�FB�9B�3B�3B�!B��B��B�XB�9B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�3B�?B�XBǮB��B��B��B�B�B�B�B�B�)B�)B�5B�ZB�B�B��B��B	B	B		7B	�B	�B	�B	%�B	#�B	%�B	)�B	+B	,B	1'B	33B	2-B	5?B	?}B	C�B	A�B	@�B	>wB	=qB	=qB	E�B	R�B	O�B	J�B	H�B	K�B	L�B	N�B	T�B	O�B	VB	R�B	O�B	N�B	Q�B	ZB	aHB	k�B	k�B	k�B	m�B	m�B	q�B	r�B	q�B	s�B	u�B	v�B	x�B	y�B	|�B	|�B	� B	�%B	�1B	�DB	�7B	�%B	�JB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�3B	�?B	�XB	�jB	�jB	�dB	�jB	�qB	�wB	��B	��B	��B	��B	ĜB	ȴB	ȴB	ɺB	��B	��B	��B	��B	�B	�B	�HB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
bB
�B
%�B
,B
0!B
7LB
?}B
F�B
L�B
P�B
T�B
XB
^5B
cTB
l�B
p�B
s�B
w�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
R�B
R�B
S�B
S�B
S�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
S�B
T�B
ZB
hlB
u�B
��B
�MB
ǤB
��B
�BBfB"�B4,BD�BjtB��B�~B��B�rB�B��B��BBU�Bt�Bw�B{�B�B�$B��B�B�MBĐBȩBđB�YBđBƜBBĎB�kB�<B�<B�;B�=B�/B��B�6B� BeQBV�BH�B0B��BȧB�JB�!B��B�MBs�BT�BC�B:RB9IB0B(�B'�B#�B\BB
��BB
��B
�<B
��B
ÇB
�B
�LB
q�B
Q�B
(�B
�B
�B
}B
jB

3B	��B	�B	�CB	ȮB	�5B	��B	��B	�`B	�?B	}�B	o�B	dYB	T�B	J�B	@B	0!B	�B	
<B	B��B��B�B�hB�=B�%B�B��BƫB�zB�UB�%B��B��B��B��B�B�hB�[B�KB�6B�0B�AB�AB�OB�mB��B��B��B�sB�OB�UB�6B}�Bx�By�Bw�Bw�B�B�TB�_B�VB�OB�6B�1B� B� B�B�B�0B�B�0B��B�B�yBǮB�`B�"B��B��B�vB�FB�8B�.B�KB�Bj�BaMBcWBcXBbTBaMB_AB\/BYBWBR�B^;Bk�Bl�Bj�BiBhzBgqBfjBfjBcZB^;BQ�BC�B=wB@�B?�B=xB9`B8UB;hB<nB?�BH�BO�BR�BV	B]1Bk�Bx�B�9B�eB��B��B��B��B��B��B��B��B��B�NB�vB�\B�EB�8B�2B�3B�!B��B��B�YB�7B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�2B�>B�VBǪB��B��B��B�B�B�B�B�B�&B�$B�3B�VB�B�B��B��B	B		B		3B	|B	�B	�B	%�B	#�B	%�B	)�B	*�B	, B	1"B	3/B	2%B	57B	?wB	C�B	A�B	@{B	>nB	=jB	=mB	E�B	R�B	O�B	J�B	H�B	K�B	L�B	N�B	T�B	O�B	U�B	R�B	O�B	N�B	Q�B	ZB	a>B	k~B	k~B	kB	m�B	m�B	q�B	r�B	q�B	s�B	u�B	v�B	x�B	y�B	|�B	|�B	�B	�B	�%B	�9B	�/B	�B	�?B	�fB	�iB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	�	B	�B	�#B	�,B	�4B	�'B	�2B	�LB	�^B	�`B	�XB	�_B	�cB	�oB	�vB	�xB	�B	�B	ĐB	ȦB	ȦB	ɰB	��B	��B	��B	��B	��B	�B	�;B	�pB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
B
UB
�B
%�B
+�B
0B
7@B
?kB
F�B
L�B
P�B
T�B
XB
^&B
cBB
lzB
p�B
s�B
w�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.15 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436422016080714364220160807143642  AO  ARCAADJP                                                                    20151215201931    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151215201931  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151215201931  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143642  IP                  G�O�G�O�G�O�                