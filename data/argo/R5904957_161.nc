CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:34Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140834  20181024140834  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @������D1   @���i�@5��`A�7�c�S���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   AffA>ffA`  A�  A�  A�  A�33A�  A�  A���A�  A�33B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�fC  C  C  C  C  C   C"�C$  C%�fC'�fC)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D��Dy�D��D� D  D�fD  D� D	  D	� D
  D
� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D%��D&� D'fD'� D(  D(� D)  D)� D*  D*y�D*��D+� D+��D,y�D-  D-� D.  D.� D/  D/� D0  D0� D1fD1� D1��D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9y�D9��D:y�D;  D;� D<  D<� D=  D=y�D>  D>� D?  D?�fD@  D@� DAfDA�fDB  DBy�DC  DC� DD  DD� DE  DEy�DE��DF� DG  DG�fDH  DHy�DH��DI� DJ  DJ�fDK  DK� DL  DL� DM  DM�fDN  DN� DO  DO� DO��DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DT��DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZ  DZ� D[  D[y�D\  D\� D]  D]� D^  D^� D_  D_�fD`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dy��D�,�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @���AffA ��A@��AbffA�33A�33A�33A�ffA�33A�33A�  A�33B 33B��B��B��B ��B(��B0��B8��BA  BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B��B�L�B�L�B�L�B�� B�� B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C &fC&fC&fC&fC&fC
&fC&fC&fC&fC&fC�C&fC&fC&fC&fC&fC &fC"@ C$&fC&�C(�C*�C,&fC.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd�Cf&fCh&fCj&fCl&fCn&fCp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�fC�fC�fC�3C�3C�fC�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�  C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�fC�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3D 	�D ��D	�D��D	�D��D	�D��D	�D��D4D�4D4D��D	�D� D	�D��D		�D	��D
	�D
��D	�D��D D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D�4D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D 	�D ��D!	�D!��D"	�D"��D#	�D#��D$	�D$��D%	�D%��D&4D&��D' D'��D(	�D(��D)	�D)��D*	�D*�4D+4D+��D,4D,�4D-	�D-��D.	�D.��D/	�D/��D0	�D0��D1 D1��D24D2��D3	�D3��D4	�D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8��D9	�D9�4D:4D:�4D;	�D;��D<	�D<��D=	�D=�4D>	�D>��D?	�D?� D@	�D@��DA DA� DB	�DB�4DC	�DC��DD	�DD��DE	�DE�4DF4DF��DG	�DG� DH	�DH�4DI4DI��DJ	�DJ� DK	�DK��DL	�DL��DM	�DM� DN	�DN��DO	�DO��DP4DP��DQ	�DQ��DR	�DR��DS	�DS��DT	�DT��DU4DU��DV	�DV��DW	�DW��DX	�DX��DY	�DY� DZ	�DZ��D[	�D[�4D\	�D\��D]	�D]��D^	�D^��D_	�D_� D`	�D`��Da	�Da��Db	�Db��Dc	�Dc��Dd	�Dd��De	�De��Df	�Df��Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj��Dk	�Dk��Dl	�Dl��Dm	�Dm��Dn	�Dn��Do	�Do��Dp	�Dp��Dq	�Dq��Dr	�Dr��Ds	�Ds�4Dt	�Dt��Du	�Du��Dv	�Dv��Dw	�Dw��Dw�gDy�]D�1�D�޹111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��A��A��A��A���A���A���A�  A�  A���A�1A�\)A�ZA�G�A�bA��A�ĜAԥ�AԑhA�~�A�dZA�M�A�&�AӍPA��yA�$�AσA��yA�33A�A�VAƗ�A�l�A��A�?}A�dZA�`BA���A���A��DA��A�dZA���A���A���A�(�A���A�(�A���A�+A�;dA���A���A�;dA���A�l�A��A�33A�ZA�%A���A���A��7A�r�A�I�A��A�=qA�XA�n�A���A���A��+A�JA�7LA�t�A���A�E�A��^A�~�A��A�ƨA� �A��A��TA��wA�+A�C�A�A�v�A��A���A���A��7A�C�A�oA�t�A��A|�Ay��Awl�AvQ�As�^Aq�hAm�Ah�/Af�jAe��AeoAbr�A_S�A^�\A]/AZn�AWdZAUG�AS|�AQ�AOhsAM�TAM��AL��AK?}AJ{AI�AH��AG�#AGx�AG
=AF��AD��AB�9A?�A>�A>�9A=��A<�A9�A7�#A7hsA6��A5�A3XA21A1�A0��A0�A.��A,r�A*��A)oA'�;A&r�A$�A#dZA!`BA�wAr�A�AS�AS�AK�A
=A�yA��A�FA�`AbNA�PA1'A|�A�A�jA�wAz�A7LAv�AbA�7A�A��A
�`A	��A	C�A�A��A�`AVAA�`A�`A�A�wA�hA?}A ��@�dZ@�&�@�E�@�"�@��7@��;@��@�9@�ff@�9@�@�1'@땁@�
=@��@�9X@�@���@�$�@��#@��@�X@���@�j@��@��y@�$�@�p�@��@�z�@߅@���@�J@܋D@�+@��@ش9@�Z@�I�@�1@��@ׅ@�;d@�ȴ@֏\@�v�@�-@թ�@��;@�v�@�O�@�7L@мj@Ͼw@�K�@���@���@��@˶F@�C�@��@ɑh@�7L@��/@�Z@��@ǅ@�"�@�M�@�O�@��@ļj@öF@�ff@�p�@�ƨ@�|�@�o@�~�@�@�V@��@�@�x�@���@���@���@��`@���@���@��j@�r�@�I�@��@�ƨ@��@�33@��y@��+@�J@�@���@��7@�hs@�O�@�7L@�/@�&�@�V@��j@�z�@�Q�@�9X@��@��
@���@�|�@�|�@�|�@�l�@�;d@��H@���@�O�@��/@��@�j@�A�@�1'@�1@��
@�ƨ@���@�t�@�"�@�~�@��#@��@���@�K�@��j@�\)@�33@���@��!@���@�E�@��7@���@�Z@��@��;@��w@��F@���@���@��R@�^5@�@��7@�O�@���@��j@��@��D@�z�@�z�@�j@�j@�bN@�Q�@�I�@�9X@�1'@�(�@��@�o@�^5@��@�J@�@��^@���@�7L@�%@�Ĝ@��@���@�r�@�bN@�Q�@�1'@��@�\)@�o@��y@�ȴ@��R@��\@�ff@�M�@�-@�{@���@��@��T@�@���@��h@�O�@��@���@��@�Q�@�  @���@���@���@�|�@�o@��!@�n�@�5?@��@�V@��@�z�@�j@�Q�@�9X@�b@��
@��@�
=@�ff@�{@��@��T@��h@�7L@��@��j@�z�@�bN@�Z@�Q�@�(�@��@�ƨ@�S�@��@�n�@�V@�M�@�=q@��@��7@�%@���@��@�j@�bN@�Z@�Q�@�I�@�9X@�1'@� �@�1@��F@�|�@�t�@�dZ@�S�@�S�@�S�@�S�@�S�@�K�@�;d@�;d@�;d@�33@�33@�33@�33@�+@�ȴ@�^5@�-@�$�@���@�@���@��7@��@�x�@�k�@�`B@l�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��A��A��A���A���A���A�  A�  A���A�1A�\)A�ZA�G�A�bA��A�ĜAԥ�AԑhA�~�A�dZA�M�A�&�AӍPA��yA�$�AσA��yA�33A�A�VAƗ�A�l�A��A�?}A�dZA�`BA���A���A��DA��A�dZA���A���A���A�(�A���A�(�A���A�+A�;dA���A���A�;dA���A�l�A��A�33A�ZA�%A���A���A��7A�r�A�I�A��A�=qA�XA�n�A���A���A��+A�JA�7LA�t�A���A�E�A��^A�~�A��A�ƨA� �A��A��TA��wA�+A�C�A�A�v�A��A���A���A��7A�C�A�oA�t�A��A|�Ay��Awl�AvQ�As�^Aq�hAm�Ah�/Af�jAe��AeoAbr�A_S�A^�\A]/AZn�AWdZAUG�AS|�AQ�AOhsAM�TAM��AL��AK?}AJ{AI�AH��AG�#AGx�AG
=AF��AD��AB�9A?�A>�A>�9A=��A<�A9�A7�#A7hsA6��A5�A3XA21A1�A0��A0�A.��A,r�A*��A)oA'�;A&r�A$�A#dZA!`BA�wAr�A�AS�AS�AK�A
=A�yA��A�FA�`AbNA�PA1'A|�A�A�jA�wAz�A7LAv�AbA�7A�A��A
�`A	��A	C�A�A��A�`AVAA�`A�`A�A�wA�hA?}A ��@�dZ@�&�@�E�@�"�@��7@��;@��@�9@�ff@�9@�@�1'@땁@�
=@��@�9X@�@���@�$�@��#@��@�X@���@�j@��@��y@�$�@�p�@��@�z�@߅@���@�J@܋D@�+@��@ش9@�Z@�I�@�1@��@ׅ@�;d@�ȴ@֏\@�v�@�-@թ�@��;@�v�@�O�@�7L@мj@Ͼw@�K�@���@���@��@˶F@�C�@��@ɑh@�7L@��/@�Z@��@ǅ@�"�@�M�@�O�@��@ļj@öF@�ff@�p�@�ƨ@�|�@�o@�~�@�@�V@��@�@�x�@���@���@���@��`@���@���@��j@�r�@�I�@��@�ƨ@��@�33@��y@��+@�J@�@���@��7@�hs@�O�@�7L@�/@�&�@�V@��j@�z�@�Q�@�9X@��@��
@���@�|�@�|�@�|�@�l�@�;d@��H@���@�O�@��/@��@�j@�A�@�1'@�1@��
@�ƨ@���@�t�@�"�@�~�@��#@��@���@�K�@��j@�\)@�33@���@��!@���@�E�@��7@���@�Z@��@��;@��w@��F@���@���@��R@�^5@�@��7@�O�@���@��j@��@��D@�z�@�z�@�j@�j@�bN@�Q�@�I�@�9X@�1'@�(�@��@�o@�^5@��@�J@�@��^@���@�7L@�%@�Ĝ@��@���@�r�@�bN@�Q�@�1'@��@�\)@�o@��y@�ȴ@��R@��\@�ff@�M�@�-@�{@���@��@��T@�@���@��h@�O�@��@���@��@�Q�@�  @���@���@���@�|�@�o@��!@�n�@�5?@��@�V@��@�z�@�j@�Q�@�9X@�b@��
@��@�
=@�ff@�{@��@��T@��h@�7L@��@��j@�z�@�bN@�Z@�Q�@�(�@��@�ƨ@�S�@��@�n�@�V@�M�@�=q@��@��7@�%@���@��@�j@�bN@�Z@�Q�@�I�@�9X@�1'@� �@�1@��F@�|�@�t�@�dZ@�S�@�S�@�S�@�S�@�S�@�K�@�;d@�;d@�;d@�33@�33@�33@�33@�+@�ȴ@�^5@�-@�$�@���@�@���@��7@��@�x�@�k�@�`B@l�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�=B
�=B
�7B
�7B
�7B
�=B
�=B
�JB
�hB
��B
��B
��B
�PB
�B�B�B�fB�B�B�B�B�B��B��BB%B	7B
=B	7BJBJBDBhB�B%�B8RB9XB;dBA�BA�BE�BJ�BH�BF�BM�BM�BR�BXBZB\)B`BBgmBdZB`BB\)BZBVBM�BI�BF�B=qB0!B(�B(�B(�B(�B'�B&�B"�B�BhB+B��B�B�NB��B�^B��B�+B{�Br�B_;B7LB�BPB
=B1BB
��B
�mB
��B
ȴB
�jB
��B
�B
p�B
W
B
9XB
+B
�B
�B
\B
B	��B	�B	�)B	��B	��B	�uB	�bB	�DB	� B	r�B	l�B	cTB	T�B	B�B	5?B	(�B	�B	VB	1B	+B	+B	B	  B��B��B��B��B��B�B�B�ZB�5B�)B�B�
B��BɺBŢBĜBB�wB�dB�XB�LB�FB�3B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB�uB�oB�oB�hB�hB�hB�bB�bB�bB�bB�bB�\B�VB�VB�VB�VB�PB�PB�PB�VB�VB�\B�oB�{B��B��B��B��B��B��B��B��B��B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�9B�?B�FB�LB�RB�jB�qB�wB�}B�wB�}B��BĜBƨBȴBȴBɺB��B��B��B��B��B�B�
B�/B�5B�;B�BB�TB�ZB�fB�mB�B�B�B�B��B��B��B	1B	
=B	JB	\B	uB	�B	�B	�B	!�B	#�B	#�B	#�B	#�B	#�B	#�B	#�B	$�B	%�B	%�B	&�B	'�B	(�B	)�B	+B	-B	/B	0!B	1'B	1'B	2-B	33B	33B	33B	5?B	;dB	?}B	B�B	C�B	E�B	F�B	J�B	O�B	P�B	R�B	VB	ZB	_;B	ffB	gmB	hsB	hsB	iyB	iyB	iyB	jB	k�B	k�B	k�B	k�B	k�B	l�B	m�B	p�B	p�B	w�B	� B	�B	�B	�B	�B	�B	�B	�B	�+B	�1B	�7B	�7B	�=B	�=B	�=B	�=B	�DB	�DB	�JB	�JB	�PB	�VB	�\B	�\B	�\B	�bB	�bB	�bB	�bB	�bB	�hB	�hB	�oB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�-B	�3B	�9B	�9B	�?B	�?B	�?B	�FB	�FB	�FB	�LB	�LB	�XB	�^B	�dB	�jB	�jB	�wB	�}B	��B	��B	��B	ÖB	ŢB	ǮB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�/B	�5B	�;B	�5B	�BB	�NB	�ZB	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
%B
%B
+B
+B
+B
1B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�=B
�=B
�7B
�7B
�7B
�=B
�=B
�JB
�hB
��B
��B
��B
�PB
�B�B�B�fB�B�B�B�B�B��B��BB%B	7B
=B	7BJBJBDBhB�B%�B8RB9XB;dBA�BA�BE�BJ�BH�BF�BM�BM�BR�BXBZB\)B`BBgmBdZB`BB\)BZBVBM�BI�BF�B=qB0!B(�B(�B(�B(�B'�B&�B"�B�BhB+B��B�B�NB��B�^B��B�+B{�Br�B_;B7LB�BPB
=B1BB
��B
�mB
��B
ȴB
�jB
��B
�B
p�B
W
B
9XB
+B
�B
�B
\B
B	��B	�B	�)B	��B	��B	�uB	�bB	�DB	� B	r�B	l�B	cTB	T�B	B�B	5?B	(�B	�B	VB	1B	+B	+B	B	  B��B��B��B��B��B�B�B�ZB�5B�)B�B�
B��BɺBŢBĜBB�wB�dB�XB�LB�FB�3B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB�uB�oB�oB�hB�hB�hB�bB�bB�bB�bB�bB�\B�VB�VB�VB�VB�PB�PB�PB�VB�VB�\B�oB�{B��B��B��B��B��B��B��B��B��B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�9B�?B�FB�LB�RB�jB�qB�wB�}B�wB�}B��BĜBƨBȴBȴBɺB��B��B��B��B��B�B�
B�/B�5B�;B�BB�TB�ZB�fB�mB�B�B�B�B��B��B��B	1B	
=B	JB	\B	uB	�B	�B	�B	!�B	#�B	#�B	#�B	#�B	#�B	#�B	#�B	$�B	%�B	%�B	&�B	'�B	(�B	)�B	+B	-B	/B	0!B	1'B	1'B	2-B	33B	33B	33B	5?B	;dB	?}B	B�B	C�B	E�B	F�B	J�B	O�B	P�B	R�B	VB	ZB	_;B	ffB	gmB	hsB	hsB	iyB	iyB	iyB	jB	k�B	k�B	k�B	k�B	k�B	l�B	m�B	p�B	p�B	w�B	� B	�B	�B	�B	�B	�B	�B	�B	�+B	�1B	�7B	�7B	�=B	�=B	�=B	�=B	�DB	�DB	�JB	�JB	�PB	�VB	�\B	�\B	�\B	�bB	�bB	�bB	�bB	�bB	�hB	�hB	�oB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�-B	�3B	�9B	�9B	�?B	�?B	�?B	�FB	�FB	�FB	�LB	�LB	�XB	�^B	�dB	�jB	�jB	�wB	�}B	��B	��B	��B	ÖB	ŢB	ǮB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�/B	�5B	�;B	�5B	�BB	�NB	�ZB	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
%B
%B
+B
+B
+B
1B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.15 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140834                              AO  ARCAADJP                                                                    20181024140834    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140834  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140834  QCF$                G�O�G�O�G�O�0               