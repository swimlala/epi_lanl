CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:55Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190555  20181005190555  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i�^�91   @��jff{`@0��hr��c��/��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C.�C0�C2  C4  C6  C8  C:  C<  C>  C@  CA�fCC�fCF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch�Cj�Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cy�fC{�fC~  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C��3C�  C��C�  C�  C��3C��3C��3C��3C��3C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D y�D ��Dy�D��D� DfD� D  D� D  D� D  D� D��D� D  D�fD	  D	�fD
fD
� D  D� D��D� D  D� D  D� D  D�fDfD�fD  D� D  Dy�D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D��Dy�D  D� D   D � D!  D!�fD"  D"y�D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(fD(�fD)fD)�fD*  D*y�D+  D+�fD,fD,�fD-fD-� D.fD.�fD/  D/� D0  D0�fD1fD1�fD2fD2� D2��D3� D4  D4� D5  D5� D6  D6� D7fD7�fD8fD8�fD9  D9� D:  D:� D;  D;y�D;��D<y�D<��D=y�D>  D>�fD?fD?� D@fD@� DAfDA�fDBfDB� DC  DC�fDD  DD� DEfDE�fDFfDF�fDG  DG� DH  DH� DIfDI� DJ  DJy�DJ��DKy�DLfDL�fDMfDM� DN  DNy�DN��DOy�DP  DP�fDQfDQ�fDR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DW��DX� DY  DYy�DY��DZy�D[  D[� D\  D\� D]  D]� D^fD^� D_  D_� D`  D`� Da  Da� Db  Db�fDc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Dh��Di� Dj  Djy�Dk  Dk�fDlfDl�fDmfDm�fDn  Dny�Dn��Do� DpfDp� Dq  Dq� Dq��Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dy�)D�9HD��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��A��A$��AD��Ad��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B �
B	=qB=qB=qB!=qB)=qB1��B9=qBA=qBI=qBQ=qBY=qBa=qBi=qBq=qBy=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B̞�BО�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\CO\CO\CO\C5�C O\C"O\C$O\C&O\C(O\C*O\C,O\C.h�C0h�C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CB5�CD5�CFO\CHO\CJO\CLO\CNO\CPO\CRO\CT5�CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\Cfh�Chh�Cjh�ClO\Cn5�CpO\CrO\CtO\CvO\CxO\Cz5�C|5�C~O\C�'�C�'�C�4{C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�4{C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C��C��C�'�C�4{C�'�C�'�C��C��C��C��C��C��C�'�C�4{C�'�C��C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�4{C�4{C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D �qDqD�qDqD��D=D��D�D��D�D��D�D��DqD��D�D�=D	�D	�=D
=D
��D�D��DqD��D�D��D�D��D�D�=D=D�=D�D��D�D�qD�D��D�D��D�D��DqD��D�D��D�D��D�D��D�D�=D�D��D�D��D�D��DqD�qD�D��D �D ��D!�D!�=D"�D"�qD#�D#�=D$�D$��D%�D%��D&�D&��D'�D'��D(=D(�=D)=D)�=D*�D*�qD+�D+�=D,=D,�=D-=D-��D.=D.�=D/�D/��D0�D0�=D1=D1�=D2=D2��D3qD3��D4�D4��D5�D5��D6�D6��D7=D7�=D8=D8�=D9�D9��D:�D:��D;�D;�qD<qD<�qD=qD=�qD>�D>�=D?=D?��D@=D@��DA=DA�=DB=DB��DC�DC�=DD�DD��DE=DE�=DF=DF�=DG�DG��DH�DH��DI=DI��DJ�DJ�qDKqDK�qDL=DL�=DM=DM��DN�DN�qDOqDO�qDP�DP�=DQ=DQ�=DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DXqDX��DY�DY�qDZqDZ�qD[�D[��D\�D\��D]�D]��D^=D^��D_�D_��D`�D`��Da�Da��Db�Db�=Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��DiqDi��Dj�Dj�qDk�Dk�=Dl=Dl�=Dm=Dm�=Dn�Dn�qDoqDo��Dp=Dp��Dq�Dq��DrqDr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx �Dy� D�C4D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�A�E�A�I�A�G�A�-A�JA��yA��#A���Aǲ-Aǝ�A�bNA��AƮAƑhA�t�A�jA�5?A�1AŬAŇ+A�r�A�r�A�v�AōPAŴ9A��A�{A�dZA�S�A�9XA��TAŸRAŲ-A��A��A��jA��FA�K�A���A�t�A���A���A��A��wA�(�A���A��A�%A��jA��-A��+A��TA�9XA�5?A���A��DA�$�A�
=A�r�A��A�oA��A���A�
=A��A��7A���A�v�A��uA�`BA�C�A���A�5?A��A�z�A��/A�~�A�{A���A��9A�9XA���A�O�A�&�A�l�A�v�A�A���A�r�A�O�A���A��/A��A��PA�
=A���A�bNA{�Ax=qAv�/At1'Ap  Al�+Ai�Ah��Ag�wAd~�Ab(�Aa��Aa+A_�A^v�A]�A[K�AYVAW
=AT{AR�/AP1ALbAG��AD��AC��AC%AA��A>�yA<-A:A8��A7A6ffA5&�A3�A2�/A/�FA-x�A,�A+ƨA*jA)\)A(�A'+A$ȴA#;dA"^5A!�^A �DA�jAƨA��A�DAI�A(�A�hA�uAI�AA�-Al�AĜA?}A�A�TA�A=qA��AVAffAXA��AbNA`BA��A{A�yA1'A1A
��A	�mA	��A��Az�A��A`BAp�A��A�TA bN@�p�@��@�t�@�ȴ@��-@��@���@�I�@�dZ@�M�@�V@�  @�t�@�\@���@�;d@�J@�x�@�z�@�P@�+@噚@�7L@�D@��@⟾@�E�@�-@�b@�K�@���@�$�@݁@�Ĝ@�Q�@۶F@ٲ-@�hs@�j@׾w@���@�M�@�?}@�%@�r�@� �@��@�;d@�=q@���@щ7@�O�@��/@�ƨ@��@���@�
=@�V@�J@��@ͩ�@�G�@��@̬@�r�@��
@˝�@�\)@�33@ʸR@ʏ\@�{@�{@�@�@��@�@�
=@�+@�K�@ϝ�@�C�@��@�1'@˅@���@��@Ǖ�@ǝ�@��@�{@���@�`B@��@��/@�Ĝ@ģ�@��@��@��@�%@��@ă@�n�@�V@�{@���@�?}@�Ĝ@��@�5?@�^5@��7@�`B@�hs@�G�@���@���@�9X@��@�\)@�l�@�\)@���@���@�5?@���@��`@���@���@�-@��D@�j@�(�@��;@�o@�v�@�$�@�{@�~�@���@��H@�$�@�hs@�  @���@��-@��@�z�@�A�@�  @��@�1@� �@�(�@�(�@�t�@���@�M�@�@��@���@��@���@��`@�K�@���@�v�@�n�@�~�@�V@�E�@�E�@�-@�{@���@�J@�`B@�7L@��@��@�%@���@�;d@�K�@��@�l�@�C�@�C�@�+@��@�-@��^@�7L@��@��u@��D@�r�@�bN@�9X@��F@�\)@�
=@���@�v�@�5?@��#@�O�@���@��@�Z@���@���@�+@�o@��y@��+@��+@�v�@�@��^@�x�@��@���@��@��@��F@�l�@�\)@�o@���@���@�ff@�E�@�$�@�@��^@�G�@���@�r�@�bN@���@��w@���@�l�@�@���@���@�v�@�E�@��@��-@�`B@�7L@��`@���@�r�@�bN@��@��;@���@�S�@�+@�
=@��H@���@�n�@�$�@�@���@��h@��@��@���@��@�ƨ@�dZ@�K�@�C�@�+@���@�~�@���@���@�ff@��@��@���@�G�@�7L@���@��u@�I�@�b@��m@��
@�C�@���@�ȴ@�~�@�=q@�-@��T@�x�@�G�@�V@���@�@l,=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�A�E�A�I�A�G�A�-A�JA��yA��#A���Aǲ-Aǝ�A�bNA��AƮAƑhA�t�A�jA�5?A�1AŬAŇ+A�r�A�r�A�v�AōPAŴ9A��A�{A�dZA�S�A�9XA��TAŸRAŲ-A��A��A��jA��FA�K�A���A�t�A���A���A��A��wA�(�A���A��A�%A��jA��-A��+A��TA�9XA�5?A���A��DA�$�A�
=A�r�A��A�oA��A���A�
=A��A��7A���A�v�A��uA�`BA�C�A���A�5?A��A�z�A��/A�~�A�{A���A��9A�9XA���A�O�A�&�A�l�A�v�A�A���A�r�A�O�A���A��/A��A��PA�
=A���A�bNA{�Ax=qAv�/At1'Ap  Al�+Ai�Ah��Ag�wAd~�Ab(�Aa��Aa+A_�A^v�A]�A[K�AYVAW
=AT{AR�/AP1ALbAG��AD��AC��AC%AA��A>�yA<-A:A8��A7A6ffA5&�A3�A2�/A/�FA-x�A,�A+ƨA*jA)\)A(�A'+A$ȴA#;dA"^5A!�^A �DA�jAƨA��A�DAI�A(�A�hA�uAI�AA�-Al�AĜA?}A�A�TA�A=qA��AVAffAXA��AbNA`BA��A{A�yA1'A1A
��A	�mA	��A��Az�A��A`BAp�A��A�TA bN@�p�@��@�t�@�ȴ@��-@��@���@�I�@�dZ@�M�@�V@�  @�t�@�\@���@�;d@�J@�x�@�z�@�P@�+@噚@�7L@�D@��@⟾@�E�@�-@�b@�K�@���@�$�@݁@�Ĝ@�Q�@۶F@ٲ-@�hs@�j@׾w@���@�M�@�?}@�%@�r�@� �@��@�;d@�=q@���@щ7@�O�@��/@�ƨ@��@���@�
=@�V@�J@��@ͩ�@�G�@��@̬@�r�@��
@˝�@�\)@�33@ʸR@ʏ\@�{@�{@�@�@��@�@�
=@�+@�K�@ϝ�@�C�@��@�1'@˅@���@��@Ǖ�@ǝ�@��@�{@���@�`B@��@��/@�Ĝ@ģ�@��@��@��@�%@��@ă@�n�@�V@�{@���@�?}@�Ĝ@��@�5?@�^5@��7@�`B@�hs@�G�@���@���@�9X@��@�\)@�l�@�\)@���@���@�5?@���@��`@���@���@�-@��D@�j@�(�@��;@�o@�v�@�$�@�{@�~�@���@��H@�$�@�hs@�  @���@��-@��@�z�@�A�@�  @��@�1@� �@�(�@�(�@�t�@���@�M�@�@��@���@��@���@��`@�K�@���@�v�@�n�@�~�@�V@�E�@�E�@�-@�{@���@�J@�`B@�7L@��@��@�%@���@�;d@�K�@��@�l�@�C�@�C�@�+@��@�-@��^@�7L@��@��u@��D@�r�@�bN@�9X@��F@�\)@�
=@���@�v�@�5?@��#@�O�@���@��@�Z@���@���@�+@�o@��y@��+@��+@�v�@�@��^@�x�@��@���@��@��@��F@�l�@�\)@�o@���@���@�ff@�E�@�$�@�@��^@�G�@���@�r�@�bN@���@��w@���@�l�@�@���@���@�v�@�E�@��@��-@�`B@�7L@��`@���@�r�@�bN@��@��;@���@�S�@�+@�
=@��H@���@�n�@�$�@�@���@��h@��@��@���@��@�ƨ@�dZ@�K�@�C�@�+@���@�~�@���@���@�ff@��@��@���@�G�@�7L@���@��u@�I�@�b@��m@��
@�C�@���@�ȴ@�~�@�=q@�-@��T@�x�@�G�@�V@���@�@l,=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�B	�B	�B	��B
B
+B
JB
PB
VB
VB
VB
PB
�B
 �B
$�B
=qB
E�B
T�B
cTB
\)B
YB
YB
\)B
dZB
s�B
~�B
��B
��B
ǮB
�B
�;B
��B�B]/B�PB�?B��B�
B�sB��B��BB
=B{B�B �B"�B&�B33B:^B;dBB�BN�BYB^5B_;B_;B^5B]/B[#BZBYBS�BL�B;dB1'B+B�BVB�B��B��B�;B�;B��B��B�?B��B��B�dB�wB�?B��Bv�B\)B=qB�B{B  B
�B
��B
�-B
��B
p�B
W
B
B�B
!�B
DB	�sB	��B	ƨB	�'B	��B	�B	u�B	q�B	n�B	]/B	P�B	M�B	G�B	:^B	6FB	.B	!�B	�B	JB��B��B�fB�B��BƨBÖB��B�dB�LB�-B�B�B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B�B�B�B�B�'B�-B�-B�3B�-B�9B�XB�^B�dB�dB�dB�jB��BÖBǮB��B��B��B��B��BȴBɺB��B��B��B��BŢBÖBƨBB�wB�qB��B�}B�qB��B��B��B�dB�'B�B�B�B��B��B�3B�dB�wB�}BBǮB��B��B��B��B��B��B��B��B�
B�B�#B�B�#B�)B�)B�)B�;B�mB�yB�B�B�B�B�B�B��B��B��B��B��B	B	B	%B	1B	1B	1B		7B	DB	DB	DB	
=B		7B	
=B	PB	bB	�B	�B	�B	�B	�B	"�B	$�B	%�B	&�B	+B	,B	,B	,B	,B	-B	0!B	33B	49B	5?B	I�B	ZB	`BB	aHB	dZB	k�B	l�B	hsB	dZB	aHB	aHB	aHB	dZB	dZB	ffB	ffB	e`B	e`B	e`B	ffB	gmB	iyB	r�B	u�B	x�B	|�B	~�B	}�B	~�B	�B	�B	�B	�B	�+B	�1B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�9B	�3B	�?B	�9B	�9B	�-B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�LB	�^B	�}B	��B	��B	��B	B	��B	�}B	�qB	�qB	�}B	ÖB	ǮB	ȴB	ɺB	��B	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�)B	�#B	�#B	�#B	�)B	�/B	�/B	�5B	�NB	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B
JB
JB
PB
PB
VB
VB
\B
bB
bB
bB
hB
oB
oB
oB
oB
uB
uB
uB
oB
oB
uB
uB
{B
�B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
1[B
:^222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B	�B	�B	�B	��B
B
+B
JB
PB
VB
VB
VB
PB
�B
 �B
$�B
=qB
E�B
T�B
cTB
\)B
YB
YB
\)B
dZB
s�B
~�B
��B
��B
ǮB
�B
�;B
��B�B]/B�PB�?B��B�
B�sB��B��BB
=B{B�B �B"�B&�B33B:^B;dBB�BN�BYB^5B_;B_;B^5B]/B[#BZBYBS�BL�B;dB1'B+B�BVB�B��B��B�;B�;B��B��B�?B��B��B�dB�wB�?B��Bv�B\)B=qB�B{B  B
�B
��B
�-B
��B
p�B
W
B
B�B
!�B
DB	�sB	��B	ƨB	�'B	��B	�B	u�B	q�B	n�B	]/B	P�B	M�B	G�B	:^B	6FB	.B	!�B	�B	JB��B��B�fB�B��BƨBÖB��B�dB�LB�-B�B�B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B�B�B�B�B�'B�-B�-B�3B�-B�9B�XB�^B�dB�dB�dB�jB��BÖBǮB��B��B��B��B��BȴBɺB��B��B��B��BŢBÖBƨBB�wB�qB��B�}B�qB��B��B��B�dB�'B�B�B�B��B��B�3B�dB�wB�}BBǮB��B��B��B��B��B��B��B��B�
B�B�#B�B�#B�)B�)B�)B�;B�mB�yB�B�B�B�B�B�B��B��B��B��B��B	B	B	%B	1B	1B	1B		7B	DB	DB	DB	
=B		7B	
=B	PB	bB	�B	�B	�B	�B	�B	"�B	$�B	%�B	&�B	+B	,B	,B	,B	,B	-B	0!B	33B	49B	5?B	I�B	ZB	`BB	aHB	dZB	k�B	l�B	hsB	dZB	aHB	aHB	aHB	dZB	dZB	ffB	ffB	e`B	e`B	e`B	ffB	gmB	iyB	r�B	u�B	x�B	|�B	~�B	}�B	~�B	�B	�B	�B	�B	�+B	�1B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�9B	�3B	�?B	�9B	�9B	�-B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�LB	�^B	�}B	��B	��B	��B	B	��B	�}B	�qB	�qB	�}B	ÖB	ǮB	ȴB	ɺB	��B	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�)B	�#B	�#B	�#B	�)B	�/B	�/B	�5B	�NB	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B
JB
JB
PB
PB
VB
VB
\B
bB
bB
bB
hB
oB
oB
oB
oB
uB
uB
uB
oB
oB
uB
uB
{B
�B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
1[B
:^222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190555                              AO  ARCAADJP                                                                    20181005190555    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190555  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190555  QCF$                G�O�G�O�G�O�8000            