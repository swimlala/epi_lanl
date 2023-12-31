CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:29Z creation      
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181024140829  20181024140829  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��䪪��1   @���5�<@4��C���c���n�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @9��@�  @�  A   A   A@  Aa��A���A�  A�  A�  A�  A���A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBg33Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D fD �fD  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D	  D	� D
  D
� D  D� D  Dy�D  D� D��D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D   D �fD!  D!� D"  D"y�D"��D#� D$  D$� D$��D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,�fD-  D-� D.fD.�fD/  D/� D0  D0� D1fD1�fD2fD2� D3  D3� D4  D4y�D5  D5� D6fD6�fD7fD7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?fD?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DFy�DG  DG� DH  DH� DI  DI� DJ  DJy�DJ��DKy�DK��DLy�DM  DMy�DN  DN� DO  DO� DP  DP� DQfDQ�fDR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DWfDW� DX  DX� DY  DYy�DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Da��Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Df��Dg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dy��D�A�D�t)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @K�@���@���Az�A$z�ADz�Af{A�
>A�=qA�=qA�=qA�=qA�
>A�=qA�=qB�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�BhQ�Bp�RBy�B��\B��\B��\B��\B��\B��\B��\B�B��\B��\B��\B��\B��\B��\B��\B��\B��\Bď\Bȏ\B̏\BЏ\Bԏ\B؏\B܏\B��\B�\B�\B�\B��\B�\B��\B��\C G�CG�CG�CG�CG�C
G�CG�CG�CG�CG�CG�CG�CG�CaHCG�CG�C G�C"G�C$G�C&G�C(G�C*G�C,G�C.G�C0G�C2G�C4G�C6aHC8aHC:G�C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CT.CVG�CXG�CZG�C\G�C^G�C`G�CbG�CdG�CfG�ChG�CjG�ClG�CnG�CpaHCraHCtG�CvG�CxG�CzG�C|G�C~G�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�
C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�D RD �RD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D�RD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D �RD!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&RD&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,RD,�RD-�D-��D.RD.�RD/�D/��D0�D0��D1RD1�RD2RD2��D3�D3��D4�D4��D5�D5��D6RD6�RD7RD7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?RD?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQRDQ�RDR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DWRDW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dy��D�J�D�}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�O�A�G�A�I�A�M�A�M�A�O�A�O�A�O�A�S�A�VA�VA�VA�XA�XA�XA�ZA�\)A�\)A�\)A�^5A�^5A�\)A�ZA�O�A��
A� �A��#A�JA�1'A�ffA�JAˁA�I�A�/A�%Aʥ�A��;Aȟ�A�%A�
=Aď\A��A�x�A�hsA���A��;A��\A��A�;dA�O�A�ȴA�JA��wA���A�dZA�A�-A�A���A�oA�;dA�\)A�JA�
=A���A���A��PA�ƨA�G�A���A���A��^A�1'A�|�A�$�A�n�A�{A�%A�M�A��/A��\A�S�A��
A���A��`A��A��hA���A�p�A�&�A��A�VA�+A��`A��A}�Ay��Axr�Aw�AtbNAlĜAjĜAg�;Ae��Ac�Aa��A`��A^�AY7LAV��AU�AT�AS�AR(�AQ�APĜAO�mALn�AIK�AG�AE��AD-AC�wAB��A?��A>1A=G�A<^5A:�yA:{A9��A8�yA733A6��A6JA5��A41A0�`A.��A-A,�A+�7A*��A)�wA(��A'��A'O�A%�
A$9XA"�A!�FA!`BA!%A �jA jA�A��At�AĜA�AƨA�An�A�A�A��Ap�An�AƨA?}A+A�AȴA�PAXA��Ar�A��A�A
z�A	��A	%A��A��A��AC�AG�A%AjA��At�AA+A�A��A n�@�1'@��/@�ƨ@���@�\)@�@���@�O�@�+@�x�@���@�|�@�+@�M�@��@�^@�V@�Q�@�o@�-@�%@�@�@�I�@��@��;@�@��@���@�V@���@�A�@�A�@��@�K�@ڧ�@ٲ-@؛�@�1@�|�@�\)@�33@�
=@�`B@���@��@�ȴ@�V@�X@� �@�@�~�@�V@�7L@��@̼j@�bN@�bN@���@̼j@�z�@�Q�@�
=@���@�S�@���@���@���@Ɨ�@Ƈ+@�@Ĭ@�z�@�ƨ@\@��@���@��@�I�@�\)@�33@�E�@���@�7L@���@�b@���@�33@�
=@�M�@��@�{@�@���@�&�@�Ĝ@��9@���@��@��@���@�v�@�V@�n�@�v�@�~�@��@�hs@�r�@��
@�
=@��@��@�O�@��9@��u@���@��u@�1@��P@�t�@�K�@�;d@�+@�33@�;d@��H@��R@���@�v�@�V@�-@�{@��@��h@�`B@�?}@�&�@�V@���@��/@�r�@�A�@��@�t�@�o@���@�^5@�5?@�$�@�{@��@���@���@���@���@��^@��7@�X@�O�@�`B@�X@�&�@��@��@�Ĝ@���@�(�@���@��@��@���@��-@���@��-@��^@���@�n�@��@�O�@���@��@��u@�Q�@�  @��P@�"�@��@���@�x�@�x�@��@�  @��@��@���@���@��T@�7L@�V@��@��@��
@��F@��@��P@�t�@�l�@�S�@�K�@�K�@�C�@�+@�"�@��@�o@�o@�@���@���@��@���@���@��R@���@�M�@�{@�J@��^@�`B@�G�@�7L@�7L@�/@�&�@��@���@��@�l�@�33@���@��H@��R@��\@�E�@�-@�{@���@�`B@���@��j@��D@�z�@�(�@��@�1@�  @���@���@��F@�;d@�V@��h@��@�%@��/@��D@�r�@�j@� �@���@���@���@��P@��@�|�@�|�@�|�@�l�@�S�@�"�@�ȴ@��!@��!@���@�~�@�~�@�v�@�ff@�$�@���@�@��-@��@�/@��j@�bN@�9X@��w@��@��`@w�{@e�=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�O�A�G�A�I�A�M�A�M�A�O�A�O�A�O�A�S�A�VA�VA�VA�XA�XA�XA�ZA�\)A�\)A�\)A�^5A�^5A�\)A�ZA�O�A��
A� �A��#A�JA�1'A�ffA�JAˁA�I�A�/A�%Aʥ�A��;Aȟ�A�%A�
=Aď\A��A�x�A�hsA���A��;A��\A��A�;dA�O�A�ȴA�JA��wA���A�dZA�A�-A�A���A�oA�;dA�\)A�JA�
=A���A���A��PA�ƨA�G�A���A���A��^A�1'A�|�A�$�A�n�A�{A�%A�M�A��/A��\A�S�A��
A���A��`A��A��hA���A�p�A�&�A��A�VA�+A��`A��A}�Ay��Axr�Aw�AtbNAlĜAjĜAg�;Ae��Ac�Aa��A`��A^�AY7LAV��AU�AT�AS�AR(�AQ�APĜAO�mALn�AIK�AG�AE��AD-AC�wAB��A?��A>1A=G�A<^5A:�yA:{A9��A8�yA733A6��A6JA5��A41A0�`A.��A-A,�A+�7A*��A)�wA(��A'��A'O�A%�
A$9XA"�A!�FA!`BA!%A �jA jA�A��At�AĜA�AƨA�An�A�A�A��Ap�An�AƨA?}A+A�AȴA�PAXA��Ar�A��A�A
z�A	��A	%A��A��A��AC�AG�A%AjA��At�AA+A�A��A n�@�1'@��/@�ƨ@���@�\)@�@���@�O�@�+@�x�@���@�|�@�+@�M�@��@�^@�V@�Q�@�o@�-@�%@�@�@�I�@��@��;@�@��@���@�V@���@�A�@�A�@��@�K�@ڧ�@ٲ-@؛�@�1@�|�@�\)@�33@�
=@�`B@���@��@�ȴ@�V@�X@� �@�@�~�@�V@�7L@��@̼j@�bN@�bN@���@̼j@�z�@�Q�@�
=@���@�S�@���@���@���@Ɨ�@Ƈ+@�@Ĭ@�z�@�ƨ@\@��@���@��@�I�@�\)@�33@�E�@���@�7L@���@�b@���@�33@�
=@�M�@��@�{@�@���@�&�@�Ĝ@��9@���@��@��@���@�v�@�V@�n�@�v�@�~�@��@�hs@�r�@��
@�
=@��@��@�O�@��9@��u@���@��u@�1@��P@�t�@�K�@�;d@�+@�33@�;d@��H@��R@���@�v�@�V@�-@�{@��@��h@�`B@�?}@�&�@�V@���@��/@�r�@�A�@��@�t�@�o@���@�^5@�5?@�$�@�{@��@���@���@���@���@��^@��7@�X@�O�@�`B@�X@�&�@��@��@�Ĝ@���@�(�@���@��@��@���@��-@���@��-@��^@���@�n�@��@�O�@���@��@��u@�Q�@�  @��P@�"�@��@���@�x�@�x�@��@�  @��@��@���@���@��T@�7L@�V@��@��@��
@��F@��@��P@�t�@�l�@�S�@�K�@�K�@�C�@�+@�"�@��@�o@�o@�@���@���@��@���@���@��R@���@�M�@�{@�J@��^@�`B@�G�@�7L@�7L@�/@�&�@��@���@��@�l�@�33@���@��H@��R@��\@�E�@�-@�{@���@�`B@���@��j@��D@�z�@�(�@��@�1@�  @���@���@��F@�;d@�V@��h@��@�%@��/@��D@�r�@�j@� �@���@���@���@��P@��@�|�@�|�@�|�@�l�@�S�@�"�@�ȴ@��!@��!@���@�~�@�~�@�v�@�ff@�$�@���@�@��-@��@�/@��j@�bN@�9X@��w@��@��`@w�{@e�=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�3B�)B��B��B��B��B��B��B��B  B%B
=B�B!�B)�B2-B=qBD�BF�BG�BG�BJ�BI�BG�BE�BF�BE�BB�BB�BC�BB�B=qB2-B'�B�BB��B�B�sB�5B�
BɺB�RB�?B�!B�B��B��B��B�uB�=Bv�BaHB=qB!�B#�B+B
��B
�sB
�#B
B
�B
��B
p�B
P�B
;dB
�B
VB	��B	�B	�ZB	ɺB	��B	�%B	p�B	bNB	Q�B	I�B	B�B	6FB	%�B	�B	�B	�B	hB	JB		7B	+B	B�B�B�B�B�B�B�yB�5B�B��B��BɺB��B��B��B��B��B��B��BŢB�wB�dB�XB�RB�FB�?B�9B�3B�-B�'B�-B�9B�?B�LB�RB�XB�XB�dB�wB�wB�jB�jB�jB�XB�^B�LB�9B�B��B�hB�VB�VB�bB�hB�uB��B��B�hB�VB�VB�PB�DB�1B�+B�%B�1B�DB�1B�uB��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�'B�'B�-B�3B�3B�9B�9B�?B�RB�qB��BǮBɺB��B��B��B�#B�;B�NB�`B�B�B��B��B	B	%B	+B	B	B��B��B��B	  B	  B	B	+B	
=B	DB	VB	bB	hB	hB	�B	�B	�B	�B	�B	�B	 �B	 �B	 �B	"�B	(�B	.B	0!B	5?B	=qB	?}B	B�B	A�B	A�B	A�B	I�B	P�B	Q�B	S�B	R�B	T�B	W
B	XB	XB	[#B	^5B	^5B	_;B	_;B	_;B	aHB	dZB	jB	k�B	l�B	m�B	n�B	o�B	q�B	q�B	t�B	u�B	v�B	w�B	x�B	x�B	x�B	|�B	~�B	� B	�B	�B	�7B	�=B	�=B	�DB	�DB	�JB	�PB	�VB	�VB	�VB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�FB	�FB	�LB	�RB	�RB	�RB	�RB	�RB	�XB	�XB	�XB	�XB	�^B	�^B	�^B	�dB	�wB	�}B	��B	��B	��B	B	ÖB	ĜB	ŢB	ŢB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
1B
	7B
	7B
DB
JB
�B
CB
,q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�3B�)B��B��B��B��B��B��B��B  B%B
=B�B!�B)�B2-B=qBD�BF�BG�BG�BJ�BI�BG�BE�BF�BE�BB�BB�BC�BB�B=qB2-B'�B�BB��B�B�sB�5B�
BɺB�RB�?B�!B�B��B��B��B�uB�=Bv�BaHB=qB!�B#�B+B
��B
�sB
�#B
B
�B
��B
p�B
P�B
;dB
�B
VB	��B	�B	�ZB	ɺB	��B	�%B	p�B	bNB	Q�B	I�B	B�B	6FB	%�B	�B	�B	�B	hB	JB		7B	+B	B�B�B�B�B�B�B�yB�5B�B��B��BɺB��B��B��B��B��B��B��BŢB�wB�dB�XB�RB�FB�?B�9B�3B�-B�'B�-B�9B�?B�LB�RB�XB�XB�dB�wB�wB�jB�jB�jB�XB�^B�LB�9B�B��B�hB�VB�VB�bB�hB�uB��B��B�hB�VB�VB�PB�DB�1B�+B�%B�1B�DB�1B�uB��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�'B�'B�-B�3B�3B�9B�9B�?B�RB�qB��BǮBɺB��B��B��B�#B�;B�NB�`B�B�B��B��B	B	%B	+B	B	B��B��B��B	  B	  B	B	+B	
=B	DB	VB	bB	hB	hB	�B	�B	�B	�B	�B	�B	 �B	 �B	 �B	"�B	(�B	.B	0!B	5?B	=qB	?}B	B�B	A�B	A�B	A�B	I�B	P�B	Q�B	S�B	R�B	T�B	W
B	XB	XB	[#B	^5B	^5B	_;B	_;B	_;B	aHB	dZB	jB	k�B	l�B	m�B	n�B	o�B	q�B	q�B	t�B	u�B	v�B	w�B	x�B	x�B	x�B	|�B	~�B	� B	�B	�B	�7B	�=B	�=B	�DB	�DB	�JB	�PB	�VB	�VB	�VB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�FB	�FB	�LB	�RB	�RB	�RB	�RB	�RB	�XB	�XB	�XB	�XB	�^B	�^B	�^B	�dB	�wB	�}B	��B	��B	��B	B	ÖB	ĜB	ŢB	ŢB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
1B
	7B
	7B
DB
JB
�B
CB
,q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140829                              AO  ARCAADJP                                                                    20181024140829    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140829  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140829  QCF$                G�O�G�O�G�O�0               