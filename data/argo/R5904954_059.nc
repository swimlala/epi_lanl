CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:02Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005191702  20181005191702  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               ;A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׾$͏E1   @׾%W:۬@5\�1&��c��G�{1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      ;A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B���B�  B�  B�  B�33B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4�C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CV  CX�CZ  C\  C^  C`  Cb  Cd�Cf�Ch�Cj  Ck�fCm�fCo�fCq�fCt  Cv�Cx  Cy�fC|  C~�C�  C��3C��C��C�  C��3C�  C�  C�  C�  C��3C�  C��3C��3C�  C��C��C�  C��3C��3C��3C��3C��3C��3C��C��C��3C��fC��3C��C��C�  C�  C��C�  C��C��C�  C��3C��3C��3C��3C��3C�  C�  C�  C��C�  C�  C��C��C��3C�  C�  C��3C��C�  C��3C��3C��3C�  C�  C�  C��C��C��3C��3C��3C�  C�  C��3C��3C��3C��3C�  C��3C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C��3C��fC�  C��C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C��3C�  C�  C��C��C��3C��3C�  C�  C�  C��3C��3C��3C��3C�  C��C��3C�  C�  C��D   D � D  D�fDfD� D  D�fDfD� D��Dy�D  D�fDfD�fD��Dy�D��D	y�D
  D
� D  D� D  D�fD  D� DfD�fD��D� D  Dy�D  D�fD��Dy�D  D� D  Dy�D��D� DfD� DfDy�D�3D� D  Dy�DfDy�D  D� D��D� DfD� D  D��DfD�fD fD �fD!fD!�fD"  D"y�D#  D#� D$  D$y�D$��D%� D&fD&�fD'fD'�fD(  D(� D(��D)� D*  D*� D+  D+y�D,fD,�fD-  D-� D.  D.� D/  D/�fD0  D0y�D1  D1� D1��D2y�D3  D3�fD4fD4� D4��D5� D6fD6� D6��D7� D8fD8� D9  DE  DE�fDF  DF�fDGfDG� DH  DH� DH��DI� DJ  DJy�DJ��DKy�DL  DL� DM  DM�fDNfDN� DO  DOy�DP  DP� DP��DQ�fDR  DR� DSfDS� DS��DT� DT��DU� DVfDV� DW  DWy�DW��DXy�DX��DY� DZ  DZ� D[  D[� D\  D\�fD]fD]� D^  D^� D^��D_� D`  D`� DafDa� Db  Db�fDc  Dc� Dd  Dd� Dd��De� Df  Df� DgfDg�fDh  Dhy�Di  Di� Di��Dj� Dk  Dky�Dl  Dl�fDm  Dm� Dn  Dn� Dn��Do� Dp  Dp� Dq  Dqy�Dr  Dr�fDs  Ds� DtfDt� Du  Du�fDvfDv� Dv��Dwy�Dw��Dy�
D�/�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�Q�@��A��A$��AD��Ad��A�z�A�z�A�z�A�z�A�z�A�z�A�A�z�B=qB	=qB=qB=qB!=qB)=qB1=qB9=qB@�BI=qBQ=qBY=qBa=qBi=qBq=qBy=qB���B���B�k�B���B���B���B�k�B���B���B���B���B���B���B���B�k�B�k�B�k�BĞ�BȞ�B̞�BО�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$h�C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4h�C6h�C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRh�CTO\CVO\CXh�CZO\C\O\C^O\C`O\CbO\Cdh�Cfh�Chh�CjO\Cl5�Cn5�Cp5�Cr5�CtO\Cvh�CxO\Cz5�C|O\C~h�C�'�C��C�4{C�4{C�'�C��C�'�C�'�C�'�C�'�C��C�'�C��C��C�'�C�4{C�4{C�'�C��C��C��C��C��C��C�4{C�4{C��C�C��C�4{C�4{C�'�C�'�C�4{C�'�C�4{C�4{C�'�C��C��C��C��C��C�'�C�'�C�'�C�4{C�'�C�'�C�4{C�4{C��C�'�C�'�C��C�4{C�'�C��C��C��C�'�C�'�C�'�C�4{C�4{C��C��C��C�'�C�'�C��C��C��C��C�'�C��C�'�C�4{C�'�C�'�C�'�C�AHC�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C��C��C��C�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�AHC�'�C��C��C��C�'�C�'�C�4{C�4{C��C��C�'�C�'�C�'�C��C��C��C��C�'�C�4{C��C�'�C�'�C�4{D �D ��D�D�=D=D��D�D�=D=D��DqD�qD�D�=D=D�=DqD�qD	qD	�qD
�D
��D�D��D�D�=D�D��D=D�=DqD��D�D�qD�D�=DqD�qD�D��D�D�qDqD��D=D��D=D�qD
D��D�D�qD=D�qD�D��DqD��D=D��D�D��D=D�=D =D �=D!=D!�=D"�D"�qD#�D#��D$�D$�qD%qD%��D&=D&�=D'=D'�=D(�D(��D)qD)��D*�D*��D+�D+�qD,=D,�=D-�D-��D.�D.��D/�D/�=D0�D0�qD1�D1��D2qD2�qD3�D3�=D4=D4��D5qD5��D6=D6��D7qD7��D8=D8��D9�DE�DE�=DF�DF�=DG=DG��DH�DH��DIqDI��DJ�DJ�qDKqDK�qDL�DL��DM�DM�=DN=DN��DO�DO�qDP�DP��DQqDQ�=DR�DR��DS=DS��DTqDT��DUqDU��DV=DV��DW�DW�qDXqDX�qDYqDY��DZ�DZ��D[�D[��D\�D\�=D]=D]��D^�D^��D_qD_��D`�D`��Da=Da��Db�Db�=Dc�Dc��Dd�Dd��DeqDe��Df�Df��Dg=Dg�=Dh�Dh�qDi�Di��DjqDj��Dk�Dk�qDl�Dl�=Dm�Dm��Dn�Dn��DoqDo��Dp�Dp��Dq�Dq�qDr�Dr�=Ds�Ds��Dt=Dt��Du�Du�=Dv=Dv��DwqDw�qDxqDy��D�9�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��HA��A��A��A��A��`Aֺ^AցA�S�A�I�A�A�A�1'A��A��A�I�Aӛ�A�7LA��Aҟ�AҁA�jA�S�A�;dA��AсA�hsA�-A�x�A���A˅A�bNA�&�A���A��#Aʰ!Aʛ�Aɛ�A�XAǃAƑhAŉ7A�Q�A�ȴA�;dA�C�A�33A�jA�~�A�l�A�
=A��A���A��hA�S�A��A�E�A�C�A��DA�7LA���A�Q�A���A��9A�dZA�v�A���A���A��A�bA�t�A�&�A��;A��jA���A��A�
=A��A�9XA��A���A���A��A��A�O�A���A��TA�/A��A���A��\A��PA�A�A�ZA�1'A���A��jA��FA��/A|�jAy�AxffAvn�Au��AtAr�Ar{Aq�AmO�Ai��AiVAgƨAf�\Ad�A`(�A[�mAY�AWXAUl�AOl�ALbNAK�AH��AG/AE%ADĜAD��ADv�AB�DA@-A?XA>��A=�FA=/A<�A:z�A8�A7p�A6�+A5VA45?A3�A1
=A-�^A+dZA*��A)hsA(��A(E�A'�^A'/A&�A&�A%��A#��A!��Az�Ax�A�Av�AS�A^5A��AE�AdZA"�Av�Ax�AA�A�A��A7LA��A��A�A�A
�A
ĜA
�!A
��A
�DA	�A�AA��A~�A�AO�A`BAhsAdZA`BAhsAhsAx�AS�A5?A��A%@���@���@��#@���@�p�@�1'@�S�@�C�@�+@�o@�
=@�@�-@��y@�33@�ff@���@���@�7L@�I�@���@�@�|�@�x�@���@�u@�33@��@�t�@�-@��T@���@�p�@�&�@�1'@�C�@�{@ܼj@��@ە�@�K�@��@ڟ�@�v�@�E�@��@���@�ƨ@�t�@�S�@�K�@�+@��H@�=q@Դ9@�1@�ƨ@�33@�ȴ@ҏ\@�@���@Ѓ@��@ΰ!@�@�`B@��@���@���@�z�@�Z@�A�@��@ˮ@���@�@�r�@��@���@�$�@�V@�z�@�S�@�M�@�5?@���@���@��/@�(�@��;@��F@�C�@�J@�hs@�/@��@���@�Ĝ@�b@�S�@��@�o@��y@�-@���@�`B@�?}@���@�  @�ƨ@���@��@�l�@�\)@���@���@�ff@�@��7@�?}@�j@��w@�K�@�@��@���@�v�@�5?@�@��T@��#@��^@��h@�G�@�j@� �@�  @��@��@��@�^5@���@�`B@�&�@��@�Ĝ@���@��@�Q�@�1@��w@��@��@�\)@��+@���@�%@�z�@�Q�@��@�\)@�;d@�;d@�"�@�o@�o@�
=@�
=@�
=@��H@���@��R@�$�@��^@�?}@���@��D@��@��@�j@�9X@�  @��;@��@��@�
=@��@�-@��h@�p�@�O�@�V@���@��;@���@�dZ@�C�@�;d@�
=@��@���@��\@�E�@�J@���@�@���@�O�@�?}@�?}@�/@���@���@�I�@�9X@�(�@�b@���@�C�@���@���@�@�
=@��@��R@��!@�M�@�E�@�=q@�5?@�5?@�J@��@��#@��#@��#@��#@�@���@�X@���@� �@���@�|�@�dZ@�\)@�K�@�C�@��@���@���@�~�@�ff@�V@�M�@�-@���@���@�x�@�hs@�O�@��9@�Z@�9X@��@�  @��
@��@�+@��H@��H@�ȴ@���@��@�j@p�.1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��HA��A��A��A��A��`Aֺ^AցA�S�A�I�A�A�A�1'A��A��A�I�Aӛ�A�7LA��Aҟ�AҁA�jA�S�A�;dA��AсA�hsA�-A�x�A���A˅A�bNA�&�A���A��#Aʰ!Aʛ�Aɛ�A�XAǃAƑhAŉ7A�Q�A�ȴA�;dA�C�A�33A�jA�~�A�l�A�
=A��A���A��hA�S�A��A�E�A�C�A��DA�7LA���A�Q�A���A��9A�dZA�v�A���A���A��A�bA�t�A�&�A��;A��jA���A��A�
=A��A�9XA��A���A���A��A��A�O�A���A��TA�/A��A���A��\A��PA�A�A�ZA�1'A���A��jA��FA��/A|�jAy�AxffAvn�Au��AtAr�Ar{Aq�AmO�Ai��AiVAgƨAf�\Ad�A`(�A[�mAY�AWXAUl�AOl�ALbNAK�AH��AG/AE%ADĜAD��ADv�AB�DA@-A?XA>��A=�FA=/A<�A:z�A8�A7p�A6�+A5VA45?A3�A1
=A-�^A+dZA*��A)hsA(��A(E�A'�^A'/A&�A&�A%��A#��A!��Az�Ax�A�Av�AS�A^5A��AE�AdZA"�Av�Ax�AA�A�A��A7LA��A��A�A�A
�A
ĜA
�!A
��A
�DA	�A�AA��A~�A�AO�A`BAhsAdZA`BAhsAhsAx�AS�A5?A��A%@���@���@��#@���@�p�@�1'@�S�@�C�@�+@�o@�
=@�@�-@��y@�33@�ff@���@���@�7L@�I�@���@�@�|�@�x�@���@�u@�33@��@�t�@�-@��T@���@�p�@�&�@�1'@�C�@�{@ܼj@��@ە�@�K�@��@ڟ�@�v�@�E�@��@���@�ƨ@�t�@�S�@�K�@�+@��H@�=q@Դ9@�1@�ƨ@�33@�ȴ@ҏ\@�@���@Ѓ@��@ΰ!@�@�`B@��@���@���@�z�@�Z@�A�@��@ˮ@���@�@�r�@��@���@�$�@�V@�z�@�S�@�M�@�5?@���@���@��/@�(�@��;@��F@�C�@�J@�hs@�/@��@���@�Ĝ@�b@�S�@��@�o@��y@�-@���@�`B@�?}@���@�  @�ƨ@���@��@�l�@�\)@���@���@�ff@�@��7@�?}@�j@��w@�K�@�@��@���@�v�@�5?@�@��T@��#@��^@��h@�G�@�j@� �@�  @��@��@��@�^5@���@�`B@�&�@��@�Ĝ@���@��@�Q�@�1@��w@��@��@�\)@��+@���@�%@�z�@�Q�@��@�\)@�;d@�;d@�"�@�o@�o@�
=@�
=@�
=@��H@���@��R@�$�@��^@�?}@���@��D@��@��@�j@�9X@�  @��;@��@��@�
=@��@�-@��h@�p�@�O�@�V@���@��;@���@�dZ@�C�@�;d@�
=@��@���@��\@�E�@�J@���@�@���@�O�@�?}@�?}@�/@���@���@�I�@�9X@�(�@�b@���@�C�@���@���@�@�
=@��@��R@��!@�M�@�E�@�=q@�5?@�5?@�J@��@��#@��#@��#@��#@�@���@�X@���@� �@���@�|�@�dZ@�\)@�K�@�C�@��@���@���@�~�@�ff@�V@�M�@�-@���@���@�x�@�hs@�O�@��9@�Z@�9X@��@�  @��
@��@�+@��H@��H@�ȴ@���@��@�j@p�.1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�=B�DB�DB�JB�DB�\B��B��B��B��B��B��B��B�B�XBBȴB��B��B��B��B��B��B��B��B�
B�B�HB�fB�B�B��B��B��B��B  BuB'�B5?BB�BG�BT�B}�B��B��B��B��B��B�B�FB�XB�dB�}B��B��B�qB�?B�!B�B��B�hB�7Bz�BffBF�B(�B�B�B	7BB��B��B��B��B�B�B�BB��B�dB��B��B�uB�hB�VB�%B|�B^5B<jB(�BoB
��B
�BB
�dB
��B
��B
�uB
�1B
n�B
I�B
5?B
)�B
�B
�B

=B
B	��B	�B	�#B	��B	�dB	�-B	��B	��B	~�B	cTB	R�B	F�B	8RB	�B	
=B	B��B�B�sB�mB�fB�`B�;B��B��B��B��BǮBĜB�wB�^B�LB�9B�!B�B�B��B��B��B�{B�oB�uB�{B��B��B��B��B��B��B�oB�+B� B|�B{�Bv�Br�Bm�BgmBcTBbNB_;B\)BZBYBXBW
BT�BT�BYB^5B^5B^5B^5B]/B\)B]/BZBXBW
BVBXB\)BcTBk�Bt�B}�B�B�%B�1B�7B�\B�PB�%B�B�B�B�7B�PB�bB��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�9B�LB�XB�^B�^B�dB�jB�qB�wB�wB�}B��BÖBÖBÖBÖBÖBĜBŢBɺB��B��B��B��B��B��B��B��B�B�#B�/B�;B�BB�HB�HB�HB�NB�NB�TB�TB�`B�mB�B�B�B��B��B��B	  B	B	B	B	B	+B	DB	JB	PB	VB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	&�B	&�B	+B	.B	/B	0!B	1'B	1'B	1'B	33B	49B	6FB	:^B	;dB	=qB	B�B	F�B	I�B	L�B	L�B	M�B	O�B	R�B	T�B	W
B	XB	^5B	bNB	cTB	jB	k�B	l�B	l�B	n�B	r�B	x�B	{�B	�B	�B	�B	�%B	�1B	�DB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�/B	�)B	�#B	�#B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�/B	�5B	�;B	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
�B
�B
#T2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B�=B�DB�DB�JB�DB�\B��B��B��B��B��B��B��B�B�XBBȴB��B��B��B��B��B��B��B��B�
B�B�HB�fB�B�B��B��B��B��B  BuB'�B5?BB�BG�BT�B}�B��B��B��B��B��B�B�FB�XB�dB�}B��B��B�qB�?B�!B�B��B�hB�7Bz�BffBF�B(�B�B�B	7BB��B��B��B��B�B�B�BB��B�dB��B��B�uB�hB�VB�%B|�B^5B<jB(�BoB
��B
�BB
�dB
��B
��B
�uB
�1B
n�B
I�B
5?B
)�B
�B
�B

=B
B	��B	�B	�#B	��B	�dB	�-B	��B	��B	~�B	cTB	R�B	F�B	8RB	�B	
=B	B��B�B�sB�mB�fB�`B�;B��B��B��B��BǮBĜB�wB�^B�LB�9B�!B�B�B��B��B��B�{B�oB�uB�{B��B��B��B��B��B��B�oB�+B� B|�B{�Bv�Br�Bm�BgmBcTBbNB_;B\)BZBYBXBW
BT�BT�BYB^5B^5B^5B^5B]/B\)B]/BZBXBW
BVBXB\)BcTBk�Bt�B}�B�B�%B�1B�7B�\B�PB�%B�B�B�B�7B�PB�bB��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�9B�LB�XB�^B�^B�dB�jB�qB�wB�wB�}B��BÖBÖBÖBÖBÖBĜBŢBɺB��B��B��B��B��B��B��B��B�B�#B�/B�;B�BB�HB�HB�HB�NB�NB�TB�TB�`B�mB�B�B�B��B��B��B	  B	B	B	B	B	+B	DB	JB	PB	VB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	&�B	&�B	+B	.B	/B	0!B	1'B	1'B	1'B	33B	49B	6FB	:^B	;dB	=qB	B�B	F�B	I�B	L�B	L�B	M�B	O�B	R�B	T�B	W
B	XB	^5B	bNB	cTB	jB	k�B	l�B	l�B	n�B	r�B	x�B	{�B	�B	�B	�B	�%B	�1B	�DB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�/B	�)B	�#B	�#B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�/B	�5B	�;B	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
�B
�B
#T2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191702                              AO  ARCAADJP                                                                    20181005191702    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191702  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191702  QCF$                G�O�G�O�G�O�8000            