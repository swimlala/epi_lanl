CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:39Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181024140839  20181024140839  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��d�=!�1   @��e""4v@5������c�?|�h1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C �C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  Cu�fCx  Cz  C|  C~  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D �fD  D� D  Dy�D  D� D  Dy�D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  Dy�D  D� D  D� D  D� D fD � D ��D!y�D!��D"y�D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2�fD3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=y�D>  D>�fD?fD?�fD@  D@� DA  DA� DA��DBy�DB��DC� DD  DD� DE  DE� DFfDF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DMy�DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDUfDU�fDV  DV� DW  DW� DX  DX� DY  DYy�DY��DZ� D[  D[� D\  D\� D]  D]� D^  D^� D^��D_y�D_��D`� D`��Da� Db  Db� Dc  Dc� DdfDd� De  De� Df  Df� DgfDg�fDhfDh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dy��D�0 D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@���AffA"ffAD  AbffA�33A�33A�33A�33A�33A�33A�33A�  B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�� B�� B�L�B�L�B��B��B�L�B�L�B�L�B�L�B�L�B�L�BЀ B�L�B�L�B܀ B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C &fC&fC&fC&fC&fC
&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC@ C @ C"&fC$&fC&@ C(&fC*&fC,&fC.&fC0&fC2&fC4&fC6&fC8&fC:@ C<&fC>&fC@&fCB&fCD&fCF&fCv�Cx&fCz&fC|&fC~&fC�3C�3C�fC�fC�3C�3C�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�fC�fC�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�fC�3C�3C�fC�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�fC�3C�3C�3C�3C�3C�3C�fC�3C�3C�3C�3C�3C�3C�3C�  C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D � D	�D��D	�D�4D	�D��D	�D�4D	�D��D	�D��D	�D��D	�D��D		�D	��D
	�D
��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D�4D	�D� D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D D��D	�D�4D	�D��D	�D��D	�D��D  D ��D!4D!�4D"4D"�4D#	�D#� D$	�D$��D%	�D%��D&	�D&��D'	�D'��D(	�D(��D)	�D)��D*	�D*��D+	�D+��D,	�D,��D-	�D-��D.	�D.��D/	�D/��D0	�D0��D1	�D1��D2	�D2� D3	�D3��D4	�D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8��D9	�D9��D:	�D:��D;	�D;��D<	�D<��D=	�D=�4D>	�D>� D? D?� D@	�D@��DA	�DA��DB4DB�4DC4DC��DD	�DD��DE	�DE��DF DF��DG	�DG��DH	�DH��DI	�DI��DJ	�DJ��DK	�DK��DL	�DL��DM	�DM�4DN	�DN��DO	�DO��DP	�DP��DQ	�DQ��DR	�DR��DS	�DS��DT	�DT� DU DU� DV	�DV��DW	�DW��DX	�DX��DY	�DY�4DZ4DZ��D[	�D[��D\	�D\��D]	�D]��D^	�D^��D_4D_�4D`4D`��Da4Da��Db	�Db��Dc	�Dc��Dd Dd��De	�De��Df	�Df��Dg Dg� Dh Dh� Di	�Di��Dj	�Dj��Dk	�Dk��Dl	�Dl��Dm	�Dm� Dn	�Dn��Do	�Do��Dp	�Dp��Dq	�Dq��Dr	�Dr��Ds	�Ds��Dt	�Dt��Du	�Du��Dv	�Dv��Dw	�Dw��Dx4Dy�gD�4�D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�"�A�$�A�&�A�1'A�5?A�7LA�9XA�;dA�;dA�=qA�?}A�?}A�A�A�A�A�A�A�A�A�A�A�C�A�C�A�C�A�C�A�E�A�G�A�G�A�"�A;wA͗�A�bNA�v�Aˇ+A��A�JA�  A��TA�t�A�&�A�%A���A�M�AȬA�;dA���A���A��`A��A��jA��PA�33A��A�&�A�l�A�1A��+A���A�dZA��^A�"�A��wA���A���A�^5A�I�A���A���A��RA��#A���A��\A��A��A��7A�dZA�S�A�C�A�;dA�E�A�G�A�A��^A�t�A���A��A�\)A�A�A� �A�M�A��uA��9A�?}A��A���A�ĜA�33A�{A���A��A���A�jAjn�Ah�Af=qAb��A`�A`n�A_�A\��AZ��AZ5?AW�AVbNAUoAT�uAS�ASXASVAR�DAQ�7AQXAQ&�AP�+AM?}AJE�AG��AE\)AEAD�RADE�AC�AAA?��A>~�A<��A9dZA7oA5�A3��A2��A1A1S�A1;dA133A1&�A/�A+t�A*ZA)�#A)C�A(�+A'x�A&�RA&VA&I�A&9XA%XA"jA!/A �DA�AhsA+A��A5?A�#AhsA33AȴA��AbA�A�A$�A�A�A
=AE�A�DAoAl�A
�A
~�A
I�A
JA	�mA	��A	��A	��A	S�A	/A�HA�+A�AoAJAE�A Ĝ@�+@��@��@��F@��H@�V@��`@��@�j@���@���@�u@�ƨ@�n�@�h@�@�@��@��
@�/@�b@�$�@��T@�p�@��;@թ�@�I�@ӝ�@�\)@�+@�-@с@�%@���@�z�@�(�@��m@�t�@��@�z�@�j@�ȴ@�=q@�?}@ċD@�Z@�(�@�ƨ@�33@�p�@�  @���@�@���@�r�@���@�dZ@�+@�+@��y@��-@��@�;d@���@�5?@�J@�&�@���@���@���@�  @��@�7L@��@���@��y@�9X@���@�33@�1'@�b@��
@���@�x�@�j@�|�@�C�@��H@��R@��-@�z�@��@�X@�=q@�$�@�p�@��/@�  @���@�dZ@�;d@�"�@��y@���@�n�@�M�@�J@��-@�p�@�/@��@��D@�b@��
@���@���@���@��@�S�@��@���@�X@�&�@�Ĝ@�(�@� �@�(�@�1'@�1'@�1@�ƨ@���@�5?@��^@�hs@���@���@�9X@��F@��y@�E�@�x�@�7L@�7L@���@��@���@�(�@��m@��@���@��@���@���@�A�@�(�@�9X@�A�@�b@��@��P@�t�@�K�@�
=@���@�~�@�5?@��#@��^@��h@�O�@��@��D@�1'@��@���@��@��P@�\)@�K�@�"�@���@��+@�ff@�$�@�{@���@�O�@��@�V@��@�j@�9X@��w@��@��@���@�;d@���@��@�hs@��@�%@��@�Ĝ@���@��u@�j@�I�@�1'@��@��F@���@�dZ@�33@�"�@�
=@�ȴ@���@�~�@�E�@�5?@�5?@��#@��@��D@��m@���@�|�@�dZ@�S�@�33@�@�ȴ@���@�^5@�n�@�V@�M�@�5?@�$�@�$�@�5?@�M�@�=q@��7@�G�@��j@�Z@��@��@�dZ@�@��H@���@�
=@��@��R@�~�@�$�@�J@�@��@�hs@���@���@��@��H@�ȴ@�ȴ@��R@��!@���@���@���@���@�~�@�+�@xĜ@m \1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�"�A�$�A�&�A�1'A�5?A�7LA�9XA�;dA�;dA�=qA�?}A�?}A�A�A�A�A�A�A�A�A�A�A�C�A�C�A�C�A�C�A�E�A�G�A�G�A�"�A;wA͗�A�bNA�v�Aˇ+A��A�JA�  A��TA�t�A�&�A�%A���A�M�AȬA�;dA���A���A��`A��A��jA��PA�33A��A�&�A�l�A�1A��+A���A�dZA��^A�"�A��wA���A���A�^5A�I�A���A���A��RA��#A���A��\A��A��A��7A�dZA�S�A�C�A�;dA�E�A�G�A�A��^A�t�A���A��A�\)A�A�A� �A�M�A��uA��9A�?}A��A���A�ĜA�33A�{A���A��A���A�jAjn�Ah�Af=qAb��A`�A`n�A_�A\��AZ��AZ5?AW�AVbNAUoAT�uAS�ASXASVAR�DAQ�7AQXAQ&�AP�+AM?}AJE�AG��AE\)AEAD�RADE�AC�AAA?��A>~�A<��A9dZA7oA5�A3��A2��A1A1S�A1;dA133A1&�A/�A+t�A*ZA)�#A)C�A(�+A'x�A&�RA&VA&I�A&9XA%XA"jA!/A �DA�AhsA+A��A5?A�#AhsA33AȴA��AbA�A�A$�A�A�A
=AE�A�DAoAl�A
�A
~�A
I�A
JA	�mA	��A	��A	��A	S�A	/A�HA�+A�AoAJAE�A Ĝ@�+@��@��@��F@��H@�V@��`@��@�j@���@���@�u@�ƨ@�n�@�h@�@�@��@��
@�/@�b@�$�@��T@�p�@��;@թ�@�I�@ӝ�@�\)@�+@�-@с@�%@���@�z�@�(�@��m@�t�@��@�z�@�j@�ȴ@�=q@�?}@ċD@�Z@�(�@�ƨ@�33@�p�@�  @���@�@���@�r�@���@�dZ@�+@�+@��y@��-@��@�;d@���@�5?@�J@�&�@���@���@���@�  @��@�7L@��@���@��y@�9X@���@�33@�1'@�b@��
@���@�x�@�j@�|�@�C�@��H@��R@��-@�z�@��@�X@�=q@�$�@�p�@��/@�  @���@�dZ@�;d@�"�@��y@���@�n�@�M�@�J@��-@�p�@�/@��@��D@�b@��
@���@���@���@��@�S�@��@���@�X@�&�@�Ĝ@�(�@� �@�(�@�1'@�1'@�1@�ƨ@���@�5?@��^@�hs@���@���@�9X@��F@��y@�E�@�x�@�7L@�7L@���@��@���@�(�@��m@��@���@��@���@���@�A�@�(�@�9X@�A�@�b@��@��P@�t�@�K�@�
=@���@�~�@�5?@��#@��^@��h@�O�@��@��D@�1'@��@���@��@��P@�\)@�K�@�"�@���@��+@�ff@�$�@�{@���@�O�@��@�V@��@�j@�9X@��w@��@��@���@�;d@���@��@�hs@��@�%@��@�Ĝ@���@��u@�j@�I�@�1'@��@��F@���@�dZ@�33@�"�@�
=@�ȴ@���@�~�@�E�@�5?@�5?@��#@��@��D@��m@���@�|�@�dZ@�S�@�33@�@�ȴ@���@�^5@�n�@�V@�M�@�5?@�$�@�$�@�5?@�M�@�=q@��7@�G�@��j@�Z@��@��@�dZ@�@��H@���@�
=@��@��R@�~�@�$�@�J@�@��@�hs@���@���@��@��H@�ȴ@�ȴ@��R@��!@���@���@���@���@�~�@�+�@xĜ@m \1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�uB�uB�oB�oB�oB�uB�uB�oB�oB�oB�oB�oB�oB�oB�oB�uB�uB�uB�oB�oB�oB�oB�oB�oB��B�-B�dB��BhBXBs�B~�B�B�B�B�%B�B�B�=B�\B��B�B�XB�?B�RB�^B�XB�XB�LB�RB�FB�?B�?B�LB�LB�XB��BƨBǮBŢB�qB�LB��B��B�{B�PB�JB�=B�Bt�Bm�Bm�Bm�Bm�Bm�BffBYBP�BG�B2-B!�B  B�B�B��B�3B��B��B��B��B��B�uB�DB|�Bk�BH�B>wB~�B	��B	ŢB	�XB	��B	��B	��B	��B	�hB	�+B	�B	w�B	n�B	gmB	cTB	^5B	ZB	XB	T�B	O�B	M�B	J�B	E�B	5?B	-B	�B	uB	oB	hB	JB	+B��B�B�mB�/B��BƨB��B�?B�-B�9B�?B�LB�LB�XB�LB�B��B��B��B��B��B��B��B��B�uB�\B�=B�+B�B�B�B�B� B~�B~�B}�B|�B{�B{�By�Bx�Bw�Bv�Bw�Bw�Bv�Bs�Bq�Bs�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bp�Bp�Bp�Bo�Bn�Bm�Bk�BhsBcTBZBR�BO�BN�BM�BL�BK�BH�BF�BB�B=qB@�BF�BG�BI�BL�BP�BW
BXBT�BO�BO�BW
B^5BffBffBe`BffBffBffBgmBiyBk�Bk�Bl�Bm�Bn�Bn�Bo�Bp�Bs�Bz�B|�B|�B}�B}�B}�B}�B}�B|�B|�B{�B{�B|�B~�B� B�B�B�B�B�B�+B�=B�VB�oB��B��B��B��B��B��B��B�!B�^BÖB��B��B�B�B��B	1B		7B		7B		7B		7B		7B	1B		7B	PB	hB	oB	�B	/B	;dB	D�B	G�B	N�B	P�B	VB	YB	[#B	\)B	]/B	_;B	aHB	cTB	e`B	ffB	gmB	hsB	jB	m�B	q�B	u�B	x�B	x�B	y�B	z�B	z�B	{�B	|�B	�B	�+B	�+B	�+B	�7B	�7B	�7B	�7B	�7B	�7B	�7B	�=B	�JB	�PB	�PB	�PB	�VB	�VB	�PB	�VB	�VB	�\B	�bB	�hB	�{B	��B	��B	��B	��B	�{B	�uB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�9B	�FB	�FB	�LB	�RB	�dB	�jB	�qB	�wB	�wB	�wB	�}B	ĜB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�#B	�)B	�/B	�5B	�BB	�TB	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
B
�B
B
'1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�uB�uB�oB�oB�oB�uB�uB�oB�oB�oB�oB�oB�oB�oB�oB�uB�uB�uB�oB�oB�oB�oB�oB�oB��B�-B�dB��BhBXBs�B~�B�B�B�B�%B�B�B�=B�\B��B�B�XB�?B�RB�^B�XB�XB�LB�RB�FB�?B�?B�LB�LB�XB��BƨBǮBŢB�qB�LB��B��B�{B�PB�JB�=B�Bt�Bm�Bm�Bm�Bm�Bm�BffBYBP�BG�B2-B!�B  B�B�B��B�3B��B��B��B��B��B�uB�DB|�Bk�BH�B>wB~�B	��B	ŢB	�XB	��B	��B	��B	��B	�hB	�+B	�B	w�B	n�B	gmB	cTB	^5B	ZB	XB	T�B	O�B	M�B	J�B	E�B	5?B	-B	�B	uB	oB	hB	JB	+B��B�B�mB�/B��BƨB��B�?B�-B�9B�?B�LB�LB�XB�LB�B��B��B��B��B��B��B��B��B�uB�\B�=B�+B�B�B�B�B� B~�B~�B}�B|�B{�B{�By�Bx�Bw�Bv�Bw�Bw�Bv�Bs�Bq�Bs�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bp�Bp�Bp�Bo�Bn�Bm�Bk�BhsBcTBZBR�BO�BN�BM�BL�BK�BH�BF�BB�B=qB@�BF�BG�BI�BL�BP�BW
BXBT�BO�BO�BW
B^5BffBffBe`BffBffBffBgmBiyBk�Bk�Bl�Bm�Bn�Bn�Bo�Bp�Bs�Bz�B|�B|�B}�B}�B}�B}�B}�B|�B|�B{�B{�B|�B~�B� B�B�B�B�B�B�+B�=B�VB�oB��B��B��B��B��B��B��B�!B�^BÖB��B��B�B�B��B	1B		7B		7B		7B		7B		7B	1B		7B	PB	hB	oB	�B	/B	;dB	D�B	G�B	N�B	P�B	VB	YB	[#B	\)B	]/B	_;B	aHB	cTB	e`B	ffB	gmB	hsB	jB	m�B	q�B	u�B	x�B	x�B	y�B	z�B	z�B	{�B	|�B	�B	�+B	�+B	�+B	�7B	�7B	�7B	�7B	�7B	�7B	�7B	�=B	�JB	�PB	�PB	�PB	�VB	�VB	�PB	�VB	�VB	�\B	�bB	�hB	�{B	��B	��B	��B	��B	�{B	�uB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�9B	�FB	�FB	�LB	�RB	�dB	�jB	�qB	�wB	�wB	�wB	�}B	ĜB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�#B	�)B	�/B	�5B	�BB	�TB	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
B
�B
B
'1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.15 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140839                              AO  ARCAADJP                                                                    20181024140839    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140839  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140839  QCF$                G�O�G�O�G�O�0               