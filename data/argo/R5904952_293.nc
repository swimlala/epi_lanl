CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:12Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190612  20181005190612  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              %A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @����y�1   @��8� @13�����c��/��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     %A   A   A   @@  @�  @�  @���AffA>ffA^ffA�  A���A�  A�  A�  A�  A�  A�33B   B  B��B  B   B(  B/��B8  B@  BH  BP  BX  B`ffBhffBp  BxffB�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�fC
  C  C  C  C  C  C  C�C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��D fD �fDfD� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	�fD
  D
� D  D� D  Dy�D  D� D  D� D  D� DfD�fD��Dy�D  D� D  D� D  D� DfD�fD  D� D  D�fD  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D��Dy�D   D �fD!  D!� D!��D"y�D"��D#y�D$  D$y�D$��D%� D&  D&� D'  D'� D'��D(y�D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/fD/�fD/��D0y�D0��D1� D1��D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8�fD9fD9� D9��D:y�D;  D;� D<  D<� D=  D=�fD>fD>�fD?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DKy�DK��DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ�fDR  DR� DS  DS� DTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D]��D^� D_  D_� D`  D`�fDa  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Df��Dg� DhfDh� Di  Di� Dj  Dj� Dj��Dky�Dl  Dl� Dl��Dmy�Dn  Dn�fDo  Doy�Dp  Dp�fDqfDq� Dq��Dr� DsfDs�fDt  Dty�Du  Du� Dv  Dvy�DwfDw�fDw��Dy� D�5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @S�
@��@��A\)A#\)AC\)Ac\)A�z�A�G�A�z�A�z�A�z�A�z�A�z�A�B=qB	=qB�B=qB!=qB)=qB0�B9=qBA=qBI=qBQ=qBY=qBa��Bi��Bq=qBy��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B̞�BО�BԞ�B؞�B���B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\C5�C
O\CO\CO\CO\CO\CO\CO\Ch�Ch�CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:h�C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\Cth�CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�4{C�'�C�'�C�'�C�4{C�4{C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�4{D =D �=D=D��DqD��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	�=D
�D
��D�D��D�D�qD�D��D�D��D�D��D=D�=DqD�qD�D��D�D��D�D��D=D�=D�D��D�D�=D�D��D�D�qD�D��D�D��D�D��D�D��D�D��DqD�qD �D �=D!�D!��D"qD"�qD#qD#�qD$�D$�qD%qD%��D&�D&��D'�D'��D(qD(�qD)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/=D/�=D0qD0�qD1qD1��D2qD2�qD3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8�=D9=D9��D:qD:�qD;�D;��D<�D<��D=�D=�=D>=D>�=D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF�=DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK�qDLqDL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ�=DR�DR��DS�DS��DT=DT�=DU=DU�=DV=DV�=DW=DW�=DX=DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^qD^��D_�D_��D`�D`�=Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��DgqDg��Dh=Dh��Di�Di��Dj�Dj��DkqDk�qDl�Dl��DmqDm�qDn�Dn�=Do�Do�qDp�Dp�=Dq=Dq��DrqDr��Ds=Ds�=Dt�Dt�qDu�Du��Dv�Dv�qDw=Dw�=Dw�Dy��D�?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�M�A�K�A�K�A�K�A�G�A�=qA�M�A�K�A�M�A�M�A�K�A�M�A�K�A�S�A�O�A�O�A�S�A�ZA�\)A�\)A�^5A�^5A�`BA�bNA�bNA�l�A�r�A�v�A�z�ÃẢ7A̍PA���A��/A�n�A�bNA�S�A��A��mA���A��A�A�jA��A��A�dZA�{A���AÃA�M�A���A¡�A�{A�1A��A�\)A�  A��hA��`A� �A���A���A��A�\)A�G�A���A�bNA�ƨA�I�A�n�A��mA��+A�7LA�
=A�C�A�VA��TA�l�A�ffA��9A��yA��yA�x�A��#A�7LA�x�A�n�A�O�A�ĜA��A��A�/A�v�A���A�ƨA��/A���A���A���A���A��A��-A��yA�t�A�^5A���A��7A��9A��wA�&�A�1'A�A��A���A���A��yA}Ay��AtApbAk�#Ai��Ah��Act�A]%AYO�AV��AU�hATĜAR �AO�FAN1'ALffAJJAG��AF�AC�hAB�jA@ȴA?�mA?A<�uA:�\A9��A8��A7K�A7A5�#A4��A2bNA1�A09XA/p�A.�jA-ƨA,��A,5?A+�A)��A'��A%��A$bNA#|�A"��A!��A ��A�TA
=A�A&�A�TAE�A�/A1AAQ�AS�Ar�A=qAl�A��A5?A/An�AA�TAl�A�yA�\A1'A�AA
��A	��A��A��AO�AoA�AA1'A��A�A/A�A��A$�A��Ax�AC�A ��A 1@��@�$�@���@��@�C�@��@�M�@��@��`@�Ĝ@��h@�=q@�@�&�@�Ĝ@���@���@���@��T@��T@���@��@���@��H@�@�X@�7L@�hs@�j@���@�o@�@���@�p�@�7L@��@�9X@�;d@��@�n�@�=q@�@�O�@�1'@�K�@�@◍@��T@���@�x�@���@�Q�@ߝ�@�\)@�
=@ޏ\@�~�@���@ݡ�@�G�@�&�@ܣ�@۝�@��y@ڧ�@ڏ\@ڇ+@�ff@�M�@�J@���@�A�@ם�@�+@���@��@�-@�G�@��@��@�+@��H@��@�%@мj@ЋD@�A�@�1'@� �@�  @��
@�S�@Ͼw@�dZ@���@·+@�@��`@�I�@�l�@�33@��@�~�@���@�7L@ȋD@�  @Ǯ@�
=@���@�?}@�Ĝ@�  @��;@î@�S�@�n�@�@��@���@�V@��`@���@���@�x�@�%@�Ĝ@�Q�@�dZ@��H@���@���@�E�@��@��^@�O�@�&�@�Z@��@��H@��@��@�o@�"�@�t�@��@�n�@�E�@�`B@�O�@�&�@��9@�Z@� �@���@�^5@�&�@���@��9@��@�j@�z�@�z�@�r�@�A�@��@�-@�J@���@���@���@��/@���@��@�b@��;@�ƨ@��P@���@�^5@�ȴ@�~�@�=q@���@�X@��u@��@��@��P@�S�@�33@�o@�;d@��R@�J@��h@���@�l�@�~�@���@�v�@�{@��-@�p�@�?}@���@��@�b@�ƨ@���@��
@���@���@��@�@�x�@��9@�r�@�r�@���@�+@�t�@�"�@�M�@���@�{@�$�@�J@�J@��@���@�x�@�hs@�7L@��@���@��@��`@���@���@�1'@�  @���@�|�@�
=@���@��y@��y@��H@���@���@�@��@��@�@�p�@��@� �@�dZ@�K�@�ȴ@�=q@�$�@��T@��-@��7@�?}@��9@�Q�@�1'@��w@���@�@���@��-@��7@�7L@�V@��@���@�bN@�(�@��@�\)@�?@���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�M�A�K�A�K�A�K�A�G�A�=qA�M�A�K�A�M�A�M�A�K�A�M�A�K�A�S�A�O�A�O�A�S�A�ZA�\)A�\)A�^5A�^5A�`BA�bNA�bNA�l�A�r�A�v�A�z�ÃẢ7A̍PA���A��/A�n�A�bNA�S�A��A��mA���A��A�A�jA��A��A�dZA�{A���AÃA�M�A���A¡�A�{A�1A��A�\)A�  A��hA��`A� �A���A���A��A�\)A�G�A���A�bNA�ƨA�I�A�n�A��mA��+A�7LA�
=A�C�A�VA��TA�l�A�ffA��9A��yA��yA�x�A��#A�7LA�x�A�n�A�O�A�ĜA��A��A�/A�v�A���A�ƨA��/A���A���A���A���A��A��-A��yA�t�A�^5A���A��7A��9A��wA�&�A�1'A�A��A���A���A��yA}Ay��AtApbAk�#Ai��Ah��Act�A]%AYO�AV��AU�hATĜAR �AO�FAN1'ALffAJJAG��AF�AC�hAB�jA@ȴA?�mA?A<�uA:�\A9��A8��A7K�A7A5�#A4��A2bNA1�A09XA/p�A.�jA-ƨA,��A,5?A+�A)��A'��A%��A$bNA#|�A"��A!��A ��A�TA
=A�A&�A�TAE�A�/A1AAQ�AS�Ar�A=qAl�A��A5?A/An�AA�TAl�A�yA�\A1'A�AA
��A	��A��A��AO�AoA�AA1'A��A�A/A�A��A$�A��Ax�AC�A ��A 1@��@�$�@���@��@�C�@��@�M�@��@��`@�Ĝ@��h@�=q@�@�&�@�Ĝ@���@���@���@��T@��T@���@��@���@��H@�@�X@�7L@�hs@�j@���@�o@�@���@�p�@�7L@��@�9X@�;d@��@�n�@�=q@�@�O�@�1'@�K�@�@◍@��T@���@�x�@���@�Q�@ߝ�@�\)@�
=@ޏ\@�~�@���@ݡ�@�G�@�&�@ܣ�@۝�@��y@ڧ�@ڏ\@ڇ+@�ff@�M�@�J@���@�A�@ם�@�+@���@��@�-@�G�@��@��@�+@��H@��@�%@мj@ЋD@�A�@�1'@� �@�  @��
@�S�@Ͼw@�dZ@���@·+@�@��`@�I�@�l�@�33@��@�~�@���@�7L@ȋD@�  @Ǯ@�
=@���@�?}@�Ĝ@�  @��;@î@�S�@�n�@�@��@���@�V@��`@���@���@�x�@�%@�Ĝ@�Q�@�dZ@��H@���@���@�E�@��@��^@�O�@�&�@�Z@��@��H@��@��@�o@�"�@�t�@��@�n�@�E�@�`B@�O�@�&�@��9@�Z@� �@���@�^5@�&�@���@��9@��@�j@�z�@�z�@�r�@�A�@��@�-@�J@���@���@���@��/@���@��@�b@��;@�ƨ@��P@���@�^5@�ȴ@�~�@�=q@���@�X@��u@��@��@��P@�S�@�33@�o@�;d@��R@�J@��h@���@�l�@�~�@���@�v�@�{@��-@�p�@�?}@���@��@�b@�ƨ@���@��
@���@���@��@�@�x�@��9@�r�@�r�@���@�+@�t�@�"�@�M�@���@�{@�$�@�J@�J@��@���@�x�@�hs@�7L@��@���@��@��`@���@���@�1'@�  @���@�|�@�
=@���@��y@��y@��H@���@���@�@��@��@�@�p�@��@� �@�dZ@�K�@�ȴ@�=q@�$�@��T@��-@��7@�?}@��9@�Q�@�1'@��w@���@�@���@��-@��7@�7L@�V@��@���@�bN@�(�@��@�\)@�?@���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B
B
B
B
	7B
PB
{B
XB
�FB
�B
�dB
�;B
��B&�BbBJBB.BB�BE�BS�BXB_;BjBm�Bo�Bp�Br�Bl�Bv�B�B��B�dB�}B�LB�}BĜB��B�B�B�yB��BB\B{B�B�B#�B"�B$�B&�B'�B%�B#�B#�B �B�B�BhBbBPB%B��B�B�`B�BB�B��B�DBt�B[#B<jB2-B1'B/B$�B\B
��B
�5B
�B
�B
��B
��B
ÖB
�9B
��B
�1B
u�B
P�B
�B	��B	�HB	�9B	�oB	x�B	n�B	ffB	A�B	uB	  B�B�B�sB�/B�B��BŢB�RB�3B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�3B�9B�3B�'B�'B�XB�jB�}B�}B�wB�dB�LB�9B�qB�jB�^B�RB�^B�qBÖBƨB�
B�HB�fB�B��B	B	+B	
=B	DB	JB	DB	
=B		7B		7B	
=B	VB	JB	
=B	
=B	DB	PB	\B	{B	�B	�B	�B	�B	�B	 �B	'�B	0!B	1'B	.B	)�B	)�B	)�B	)�B	)�B	)�B	(�B	'�B	'�B	(�B	+B	33B	;dB	;dB	>wB	=qB	:^B	2-B	+B	%�B	)�B	2-B	B�B	M�B	[#B	hsB	iyB	k�B	n�B	m�B	s�B	x�B	y�B	w�B	�7B	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	�{B	�oB	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�3B	�9B	�9B	�?B	�FB	�RB	�XB	�dB	�qB	�wB	�wB	�wB	��B	��B	��B	ÖB	ƨB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	ɺB	ɺB	��B	��B	��B	�B	�B	��B	�B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�)B	�5B	�5B	�5B	�5B	�5B	�/B	�)B	�)B	�5B	�5B	�5B	�5B	�/B	�)B	�/B	�5B	�5B	�5B	�/B	�5B	�;B	�5B	�5B	�/B	�BB	�TB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
B
B
B
B
B
B
B
B
B
%B
+B
%B
+B
1B
	7B
	7B

=B

=B
DB
PB
PB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
\B
bB
bB
bB
bB
bB
bB
bB
hB
bB
bB
bB
bB
\B
\B
VB
PB
PB
JB
JB
JB
JB
JB
JB
DB
DB
DB

=B
	7B

=B

=B

=B
DB
DB
DB
DB
JB
JB
JB
JB
#�B
�B
 B222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B
B
B
B
	7B
PB
{B
XB
�FB
�B
�dB
�;B
��B&�BbBJBB.BB�BE�BS�BXB_;BjBm�Bo�Bp�Br�Bl�Bv�B�B��B�dB�}B�LB�}BĜB��B�B�B�yB��BB\B{B�B�B#�B"�B$�B&�B'�B%�B#�B#�B �B�B�BhBbBPB%B��B�B�`B�BB�B��B�DBt�B[#B<jB2-B1'B/B$�B\B
��B
�5B
�B
�B
��B
��B
ÖB
�9B
��B
�1B
u�B
P�B
�B	��B	�HB	�9B	�oB	x�B	n�B	ffB	A�B	uB	  B�B�B�sB�/B�B��BŢB�RB�3B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�3B�9B�3B�'B�'B�XB�jB�}B�}B�wB�dB�LB�9B�qB�jB�^B�RB�^B�qBÖBƨB�
B�HB�fB�B��B	B	+B	
=B	DB	JB	DB	
=B		7B		7B	
=B	VB	JB	
=B	
=B	DB	PB	\B	{B	�B	�B	�B	�B	�B	 �B	'�B	0!B	1'B	.B	)�B	)�B	)�B	)�B	)�B	)�B	(�B	'�B	'�B	(�B	+B	33B	;dB	;dB	>wB	=qB	:^B	2-B	+B	%�B	)�B	2-B	B�B	M�B	[#B	hsB	iyB	k�B	n�B	m�B	s�B	x�B	y�B	w�B	�7B	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	�{B	�oB	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�3B	�9B	�9B	�?B	�FB	�RB	�XB	�dB	�qB	�wB	�wB	�wB	��B	��B	��B	ÖB	ƨB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	ɺB	ɺB	��B	��B	��B	�B	�B	��B	�B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�)B	�5B	�5B	�5B	�5B	�5B	�/B	�)B	�)B	�5B	�5B	�5B	�5B	�/B	�)B	�/B	�5B	�5B	�5B	�/B	�5B	�;B	�5B	�5B	�/B	�BB	�TB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
B
B
B
B
B
B
B
B
B
%B
+B
%B
+B
1B
	7B
	7B

=B

=B
DB
PB
PB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
\B
bB
bB
bB
bB
bB
bB
bB
hB
bB
bB
bB
bB
\B
\B
VB
PB
PB
JB
JB
JB
JB
JB
JB
DB
DB
DB

=B
	7B

=B

=B

=B
DB
DB
DB
DB
JB
JB
JB
JB
#�B
�B
 B222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190612                              AO  ARCAADJP                                                                    20181005190612    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190612  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190612  QCF$                G�O�G�O�G�O�8000            