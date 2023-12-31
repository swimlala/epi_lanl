CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:17Z creation      
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
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  Sx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  f   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181005190517  20181005190517  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               5A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׼�]LM�1   @׼��K�@1^5?|��c���`A�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      5A   A   A   @@  @�  @���A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCz  C|  C~  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  D   D � DfD� D  D� D  D� D  Dy�D��D� D  D� D  D� D  D� D	  D	� D
  D
� D  D�fD  D�fDfD� D  Dy�D  D�fD  D� D  D� DfD� D  D� DfD� D��D� D  Dy�D  D� D��D� D  D� D  D� D  Dy�D  D� D��D� DfD� D��Dy�D   D � D!fD!� D"  D"� D"��D#y�D#��D$y�D$��D%y�D%��D&y�D'  D'� D(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-�fD.fD.� D/  D/� D0  D0� D1  D1�fD2fD2�fD3  D3� D4  D4� D5  D5� D6  D6� D7  D7y�D8  D8� D9fD9� D:  D:� D:��D;y�D;��D<y�D=  D=�fD>fD>� D?  D?� D@  D@� DA  DAy�DA��DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DGy�DH  DH�fDIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DN��DO� DPfDP�fDQfDQ�fDR  DRy�DS  DS�fDT  DT� DUfDU�fDVfDV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[y�D\  D\� D\��D]� D^fD^� D^��D_� D`  D`�fDa  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df�fDgfDg� Dh  Dh� Di  Di�fDjfDj�fDk  Dk� DlfDl� Dl��Dm� Dn  Dny�Do  Do�fDp  Dp� DqfDq� Dr  Dr� Ds  Ds�fDt  Dt� Du  Du� Dv  Dv� Dw  Dw�fDw� Dy�RD�C�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@S�
@��@ƸRA��A$��AD��Ad��A�z�A�z�A�z�A��A�z�A�z�A�z�A�z�B=qB	=qB�B=qB!=qB)=qB1=qB9=qBA=qBI=qBQ=qBY=qBa=qBi=qBq=qBy=qB���B�k�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B̞�BО�BԞ�B؞�Bܞ�B���B�k�B�k�B잸B�B���B���B���C O\C5�CO\CO\CO\C
O\C5�CO\CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\CtO\CvO\Cx5�CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�4{C�'�C�'�C�'�C�4{C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�4{C�4{C�'�C�'�C�'�C��C��C��C��C��C�'�C�'�C�'�C�'�C�'�D �D ��D=D��D�D��D�D��D�D�qDqD��D�D��D�D��D�D��D	�D	��D
�D
��D�D�=D�D�=D=D��D�D�qD�D�=D�D��D�D��D=D��D�D��D=D��DqD��D�D�qD�D��DqD��D�D��D�D��D�D�qD�D��DqD��D=D��DqD�qD �D ��D!=D!��D"�D"��D#qD#�qD$qD$�qD%qD%�qD&qD&�qD'�D'��D(�D(��D)�D)�=D*�D*��D+�D+��D,�D,��D-�D-�=D.=D.��D/�D/��D0�D0��D1�D1�=D2=D2�=D3�D3��D4�D4��D5�D5��D6�D6��D7�D7�qD8�D8��D9=D9��D:�D:��D;qD;�qD<qD<�qD=�D=�=D>=D>��D?�D?��D@�D@��DA�DA�qDBqDB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG�qDH�DH�=DI=DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DOqDO��DP=DP�=DQ=DQ�=DR�DR�qDS�DS�=DT�DT��DU=DU�=DV=DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[�qD\�D\��D]qD]��D^=D^��D_qD_��D`�D`�=Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df�=Dg=Dg��Dh�Dh��Di�Di�=Dj=Dj�=Dk�Dk��Dl=Dl��DmqDm��Dn�Dn�qDo�Do�=Dp�Dp��Dq=Dq��Dr�Dr��Ds�Ds�=Dt�Dt��Du�Du��Dv�Dv��Dw�Dw�=Dw��Dy�)D�M�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A��mA���Aٙ�A�n�A�1'A��A��A�bA�AؼjA�K�A��A�z�A�+A�;dA�XA�5?A��A��A�ĜAօA��A�I�A�VA�%A��`AՋDA��AԼjAӣ�A�;dA� �A���A�^5A�A�bNA���A��A��A΁A��A�ȴA�bNA̶FA�C�A˙�A�5?A���A�
=AȸRA�VA�t�AƍPAź^Aĩ�A¡�A�A�hsA��-A�
=A���A��!A��\A�5?A�p�A��9A�O�A��A�  A�K�A�$�A�\)A��A��A�I�A�ffA��jA���A�  A���A��9A��RA���A���A��PA��A��A�7LA���A��A���A�A�Q�A�
=A�%A�E�A�JA�r�A��A�"�A�z�A��TA��-A��DA�~�A�JA��A��!A��A~��A~JAz�Ay/Aw�Aux�AtJAs`BArVAqAj�!Ag��Af��Af�+Ac7LA^n�A\�RAY�;AW��AU��AP�RAM��AK&�AJ~�AI�7AF�uAEAC�A@�A=�#A�yA(�A�A�Ar�A��At�A�A��A�AS�AK�A�/An�A�TAx�A��A�^A��A��A�A%A��A��A-AA
ĜA	�mA	�^A	�mA
�\AS�A�jAE�A�Av�A  A�PA&�A�RAz�A@�~�@�K�@���@��R@�(�@��@��@�&�@� �@�M�@��^@���@��!@�@�Q�@�7L@�@�`B@�z�@��7@�
=@��@�V@�j@� �@�
=@�+@�E�@���@��#@� �@��m@�n�@�~�@��@�z�@�-@��@�O�@�b@��y@��#@��@���@��@�bN@�Q�@�b@�dZ@߶F@߶F@ߥ�@�;d@�-@��
@ڧ�@��y@���@���@�A�@�@θR@͑h@���@�?}@�ƨ@�5?@�@Ų-@��#@ļj@�r�@��@���@�\)@�;d@��@�ȴ@°!@�@§�@�@���@��^@�dZ@���@��u@�l�@�^5@�$�@���@�t�@�1@��@��@��\@�l�@�o@���@���@�ff@�x�@��u@���@���@��@�b@�\)@�@�=q@��@��D@�^5@�x�@���@�O�@��@���@��P@�ȴ@�=q@�5?@���@�x�@�7L@��7@�$�@�ff@��\@��+@��+@�V@�O�@��u@�bN@�j@��@���@�Z@�Q�@�I�@���@��`@��D@��
@���@�l�@�S�@���@�;d@�o@�V@�@�@��#@��7@�X@�X@�?}@�7L@�`B@���@��`@�G�@��j@���@�v�@�"�@��@��@�ȴ@��R@�n�@�J@�hs@���@�(�@��@��@�"�@��@�&�@�r�@�|�@�K�@�n�@��@���@��T@��@��-@�X@��9@��D@� �@��@��
@��@��w@�1'@�1'@�b@���@��^@��h@�X@�p�@��u@��@��@���@�~�@���@��@�x�@���@���@�Z@��
@��@�+@���@��+@�ff@�-@��@�{@���@��T@���@�hs@��@��j@�r�@���@���@��@�S�@�
=@�@��\@��@���@�x�@�&�@�%@���@��9@�bN@��m@�|�@�
=@���@�^5@��@��@��@�|�@��@��R@�V@�^5@�5?@�{@�J@�@��@���@���@�x�@�`B@�O�@���@�I�@�b@���@�S�@�S�@���@���@�n�@�M�@���@�&�@���@��`@���@�ߤ@��@nR�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A��mA���Aٙ�A�n�A�1'A��A��A�bA�AؼjA�K�A��A�z�A�+A�;dA�XA�5?A��A��A�ĜAօA��A�I�A�VA�%A��`AՋDA��AԼjAӣ�A�;dA� �A���A�^5A�A�bNA���A��A��A΁A��A�ȴA�bNA̶FA�C�A˙�A�5?A���A�
=AȸRA�VA�t�AƍPAź^Aĩ�A¡�A�A�hsA��-A�
=A���A��!A��\A�5?A�p�A��9A�O�A��A�  A�K�A�$�A�\)A��A��A�I�A�ffA��jA���A�  A���A��9A��RA���A���A��PA��A��A�7LA���A��A���A�A�Q�A�
=A�%A�E�A�JA�r�A��A�"�A�z�A��TA��-A��DA�~�A�JA��A��!A��A~��A~JAz�Ay/Aw�Aux�AtJAs`BArVAqAj�!Ag��Af��Af�+Ac7LA^n�A\�RAY�;AW��AU��AP�RAM��AK&�AJ~�AI�7AF�uAEAC�A@�A=�#A�yA(�A�A�Ar�A��At�A�A��A�AS�AK�A�/An�A�TAx�A��A�^A��A��A�A%A��A��A-AA
ĜA	�mA	�^A	�mA
�\AS�A�jAE�A�Av�A  A�PA&�A�RAz�A@�~�@�K�@���@��R@�(�@��@��@�&�@� �@�M�@��^@���@��!@�@�Q�@�7L@�@�`B@�z�@��7@�
=@��@�V@�j@� �@�
=@�+@�E�@���@��#@� �@��m@�n�@�~�@��@�z�@�-@��@�O�@�b@��y@��#@��@���@��@�bN@�Q�@�b@�dZ@߶F@߶F@ߥ�@�;d@�-@��
@ڧ�@��y@���@���@�A�@�@θR@͑h@���@�?}@�ƨ@�5?@�@Ų-@��#@ļj@�r�@��@���@�\)@�;d@��@�ȴ@°!@�@§�@�@���@��^@�dZ@���@��u@�l�@�^5@�$�@���@�t�@�1@��@��@��\@�l�@�o@���@���@�ff@�x�@��u@���@���@��@�b@�\)@�@�=q@��@��D@�^5@�x�@���@�O�@��@���@��P@�ȴ@�=q@�5?@���@�x�@�7L@��7@�$�@�ff@��\@��+@��+@�V@�O�@��u@�bN@�j@��@���@�Z@�Q�@�I�@���@��`@��D@��
@���@�l�@�S�@���@�;d@�o@�V@�@�@��#@��7@�X@�X@�?}@�7L@�`B@���@��`@�G�@��j@���@�v�@�"�@��@��@�ȴ@��R@�n�@�J@�hs@���@�(�@��@��@�"�@��@�&�@�r�@�|�@�K�@�n�@��@���@��T@��@��-@�X@��9@��D@� �@��@��
@��@��w@�1'@�1'@�b@���@��^@��h@�X@�p�@��u@��@��@���@�~�@���@��@�x�@���@���@�Z@��
@��@�+@���@��+@�ff@�-@��@�{@���@��T@���@�hs@��@��j@�r�@���@���@��@�S�@�
=@�@��\@��@���@�x�@�&�@�%@���@��9@�bN@��m@�|�@�
=@���@�^5@��@��@��@�|�@��@��R@�V@�^5@�5?@�{@�J@�@��@���@���@�x�@�`B@�O�@���@�I�@�b@���@�S�@�S�@���@���@�n�@�M�@���@�&�@���@��`@���@�ߤ@��@nR�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
>wB
?}B
>wB
>wB
>wB
>wB
=qB
<jB
;dB
:^B
;dB
;dB
;dB
;dB
=qB
B�B
K�B
Q�B
W
B
\)B
t�B
�=B
�oB
�JB
�B
z�B
��B
�XB
��B
�5B  B\B%BVB
�BPB�B-B6FB8RB7LB7LB&�BPB�B �B!�B�BbB�B&�BD�BI�BK�BgmBz�B��B�wB�ZB\B49BI�BJ�BP�BN�B�B�bB�hB�uB��B��B��B�!B�RB�B��B��B�B�B�-B�B��B��B�1B}�Be`BQ�B@�B%B��B�ZB�5B��B�B�7B�PB��BB��Bw�BffB7LB�B
��B
�B
�B
�B
��B
��B
�qB
�3B
�7B
'�B	��B	�B	�B	�
B	ƨB	�^B	�B	��B	��B	��B	��B	�1B	q�B	q�B	|�B	hsB	R�B	B�B	2-B	-B	#�B	1B��B�B�B�mB�5B�B��B��BŢB�FB�RB�XB�XB�dB�^B��B��B��B��B�B�mB�B�B�B�B�B�B�B�B�B�B�ZB�5B�5B�B�)B�BB�NB�`B�B��B�sB�mB�TB�
BƨB��B��B��B��B��B��B�^B�LB��B�/B�B�
B�B��B��B��B�TB�yB�sB�B��B	B	�B	�B	 �B	2-B	2-B	33B	33B	33B	1'B	0!B	1'B	#�B	VB	%B		7B	)�B	0!B	6FB	33B	0!B	1'B	33B	8RB	9XB	?}B	C�B	G�B	J�B	P�B	S�B	YB	]/B	aHB	cTB	cTB	hsB	hsB	dZB	_;B	VB	VB	Q�B	H�B	C�B	B�B	?}B	6FB	0!B	,B	&�B	'�B	,B	0!B	2-B	33B	33B	33B	49B	5?B	8RB	:^B	=qB	?}B	A�B	B�B	C�B	I�B	W
B	W
B	P�B	N�B	N�B	Q�B	XB	`BB	e`B	jB	hsB	k�B	w�B	x�B	w�B	x�B	y�B	y�B	w�B	x�B	x�B	x�B	v�B	u�B	t�B	s�B	s�B	r�B	m�B	k�B	n�B	m�B	m�B	n�B	o�B	p�B	q�B	q�B	r�B	t�B	x�B	{�B	�B	�B	�1B	�7B	�DB	�VB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�FB	�?B	�?B	�FB	�FB	�LB	�RB	�XB	�XB	�XB	�dB	�dB	�qB	B	B	�}B	�wB	ÖB	ŢB	ȴB	ɺB	��B	��B	ɺB	ȴB	ȴB	ǮB	ǮB	ȴB	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�/B	�HB	�`B	�sB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
1B
1B
1B
+B
%B
+B
1B
1B

=B

=B
DB
DB
JB
PB
PB
PB
PB
VB
PB
PB
JB
JB
JB
PB
PB
PB
JB
JB
JB
DB
PB
VB
bB
�B
7B
(>B
2-22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B
>wB
?}B
>wB
>wB
>wB
>wB
=qB
<jB
;dB
:^B
;dB
;dB
;dB
;dB
=qB
B�B
K�B
Q�B
W
B
\)B
t�B
�=B
�oB
�JB
�B
z�B
��B
�XB
��B
�5B  B\B%BVB
�BPB�B-B6FB8RB7LB7LB&�BPB�B �B!�B�BbB�B&�BD�BI�BK�BgmBz�B��B�wB�ZB\B49BI�BJ�BP�BN�B�B�bB�hB�uB��B��B��B�!B�RB�B��B��B�B�B�-B�B��B��B�1B}�Be`BQ�B@�B%B��B�ZB�5B��B�B�7B�PB��BB��Bw�BffB7LB�B
��B
�B
�B
�B
��B
��B
�qB
�3B
�7B
'�B	��B	�B	�B	�
B	ƨB	�^B	�B	��B	��B	��B	��B	�1B	q�B	q�B	|�B	hsB	R�B	B�B	2-B	-B	#�B	1B��B�B�B�mB�5B�B��B��BŢB�FB�RB�XB�XB�dB�^B��B��B��B��B�B�mB�B�B�B�B�B�B�B�B�B�B�ZB�5B�5B�B�)B�BB�NB�`B�B��B�sB�mB�TB�
BƨB��B��B��B��B��B��B�^B�LB��B�/B�B�
B�B��B��B��B�TB�yB�sB�B��B	B	�B	�B	 �B	2-B	2-B	33B	33B	33B	1'B	0!B	1'B	#�B	VB	%B		7B	)�B	0!B	6FB	33B	0!B	1'B	33B	8RB	9XB	?}B	C�B	G�B	J�B	P�B	S�B	YB	]/B	aHB	cTB	cTB	hsB	hsB	dZB	_;B	VB	VB	Q�B	H�B	C�B	B�B	?}B	6FB	0!B	,B	&�B	'�B	,B	0!B	2-B	33B	33B	33B	49B	5?B	8RB	:^B	=qB	?}B	A�B	B�B	C�B	I�B	W
B	W
B	P�B	N�B	N�B	Q�B	XB	`BB	e`B	jB	hsB	k�B	w�B	x�B	w�B	x�B	y�B	y�B	w�B	x�B	x�B	x�B	v�B	u�B	t�B	s�B	s�B	r�B	m�B	k�B	n�B	m�B	m�B	n�B	o�B	p�B	q�B	q�B	r�B	t�B	x�B	{�B	�B	�B	�1B	�7B	�DB	�VB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�FB	�?B	�?B	�FB	�FB	�LB	�RB	�XB	�XB	�XB	�dB	�dB	�qB	B	B	�}B	�wB	ÖB	ŢB	ȴB	ɺB	��B	��B	ɺB	ȴB	ȴB	ǮB	ǮB	ȴB	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�/B	�HB	�`B	�sB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
1B
1B
1B
+B
%B
+B
1B
1B

=B

=B
DB
DB
JB
PB
PB
PB
PB
VB
PB
PB
JB
JB
JB
PB
PB
PB
JB
JB
JB
DB
PB
VB
bB
�B
7B
(>B
2-22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190517                              AO  ARCAADJP                                                                    20181005190517    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190517  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181005190517  QCF$                G�O�G�O�G�O�8000            