CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:14Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005191714  20181005191714  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               qA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @�˥*��1   @�˥�s�@4�E����dWKƧ�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      qA   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   BffBffB33B��B(  B0  B8ffB@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B���B�  B�  B�  B�33B�33B�  B�  B�  C   C  C  C  C�fC
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8�C:  C<  C>  C?�fCA�fCC�fCF  CG�fCI�fCK�fCM�fCP�CR  CS�fCU�fCX  CZ  C\�C^33C`  Cb�Cd�Cf�Cg�fCj  Cl  Cn  Cp  Cr  Ct�Cv  Cx�Cz�C|  C}�fC�fC�  C��C��3C�  C�  C��C�  C�  C�  C�  C�  C��C��3C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C��3C�  C��3C��3C��3C�  C��C��3C�  C��3C��C�  C��C��C�  C�  C��C�  C��3C��3C�  C�  C�  C��C��3C��fC�  C��C��C�  C�  C�  C��3C�  C�  C��3C��3C�  C��C�  C��3C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C��C�  C��3C�  C��C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C��3C��3C�  C��C�  C�  C��C�  C�  C��3C��fC�  C�  C��C��C�  C�  C�  D   D �fDfD� DfD� D��D� D  Dy�D  Dy�D�3D� D  D� DfD� D��D	y�D	��D
� D  D� D  Dy�D��Dy�D��D� D  D� D  D� DfD�fD  Dy�DfD�fD��D� DfD�fD  D� D  Dy�D  D� D  D� D  Dy�D  D�fDfD� DfD� D  D�fD��D� D   D �fD!fD!�fD!��D"� D#  D#� D#��D$y�D%  D%� D&  D&� D'  D'� D(  D(� D)  D)y�D)��D*� D+fD+� D+��D,� D-  D-� D.  D.�fD/  D/y�D/��D0� D0��D1y�D2  D2�fD3  D3� D4  D4� D5fD5�fD6  D6y�D6��D7� D7��D8y�D9  D9� D9��D:� D;fD;� D<  D<� D=fD=� D>  D>� D?  D?�fD@fD@�fDA  DA� DBfDB�fDC  DCy�DC��DD� DEfDE�fDF  DFy�DG  DGy�DH  DH�fDIfDI� DI��DJ� DK  DKy�DL  DL� DL��DMy�DN  DN�fDOfDO� DP  DPy�DP��DQ� DRfDR�fDSfDS��DTfDT�fDUfDU�fDU��DVy�DV��DWy�DX  DX� DY  DYy�DY��DZy�D[  D[� D\  D\� D]  D]�fD^  D^y�D^��D_y�D`  D`y�D`��Da� Db  Db� Dc  Dcy�Dc��Ddy�De  De� Df  Df� DgfDg� Dg��Dh� Di  Diy�Di��Dj� Dk  Dky�Dl  Dl� Dm  Dm� Dm��Dny�Do  Do� DpfDp�fDp��Dq� Dr  Dry�Ds  Ds�fDtfDt�fDufDu� Du��Dvy�Dv��Dws3Dw��Dyz=D�7\D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @Mp�@��@��A��A$��AD��Ad��A�z�A�z�A�z�A�z�A�z�A�z�A�G�A�z�B=qB	��B��Bp�B �B)=qB1=qB9��BA=qBI=qBP�BY=qBa=qBi=qBq=qBy=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B�k�BО�BԞ�B�k�B�k�B���B䞸B螸B���B���B���B���B���C O\CO\CO\CO\C5�C
O\CO\CO\CO\Ch�CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C25�C4O\C6O\C8h�C:O\C<O\C>O\C@5�CB5�CD5�CFO\CH5�CJ5�CL5�CN5�CPh�CRO\CT5�CV5�CXO\CZO\C\h�C^��C`O\Cbh�Cdh�Cfh�Ch5�CjO\ClO\CnO\CpO\CrO\Cth�CvO\Cxh�Czh�C|O\C~5�C��C�'�C�4{C��C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�4{C��C�'�C�'�C�'�C�4{C�'�C�'�C�4{C�'�C�'�C�'�C�'�C��C�'�C��C��C��C�'�C�4{C��C�'�C��C�4{C�'�C�4{C�4{C�'�C�'�C�4{C�'�C��C��C�'�C�'�C�'�C�4{C��C�C�'�C�4{C�4{C�'�C�'�C�'�C��C�'�C�'�C��C��C�'�C�4{C�'�C��C�'�C�4{C�'�C��C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C��C��C�4{C�'�C��C�'�C�4{C�'�C�'�C�'�C�4{C�'�C��C�'�C�'�C�'�C�'�C�'�C��C�'�C�4{C�4{C�'�C�'�C��C��C�'�C�4{C�'�C�'�C�4{C�'�C�'�C��C�C�'�C�'�C�4{C�4{C�'�C�'�C�'�D �D �=D=D��D=D��DqD��D�D�qD�D�qD
D��D�D��D=D��D	qD	�qD
qD
��D�D��D�D�qDqD�qDqD��D�D��D�D��D=D�=D�D�qD=D�=DqD��D=D�=D�D��D�D�qD�D��D�D��D�D�qD�D�=D=D��D=D��D�D�=DqD��D �D �=D!=D!�=D"qD"��D#�D#��D$qD$�qD%�D%��D&�D&��D'�D'��D(�D(��D)�D)�qD*qD*��D+=D+��D,qD,��D-�D-��D.�D.�=D/�D/�qD0qD0��D1qD1�qD2�D2�=D3�D3��D4�D4��D5=D5�=D6�D6�qD7qD7��D8qD8�qD9�D9��D:qD:��D;=D;��D<�D<��D==D=��D>�D>��D?�D?�=D@=D@�=DA�DA��DB=DB�=DC�DC�qDDqDD��DE=DE�=DF�DF�qDG�DG�qDH�DH�=DI=DI��DJqDJ��DK�DK�qDL�DL��DMqDM�qDN�DN�=DO=DO��DP�DP�qDQqDQ��DR=DR�=DS=DS��DT=DT�=DU=DU�=DVqDV�qDWqDW�qDX�DX��DY�DY�qDZqDZ�qD[�D[��D\�D\��D]�D]�=D^�D^�qD_qD_�qD`�D`�qDaqDa��Db�Db��Dc�Dc�qDdqDd�qDe�De��Df�Df��Dg=Dg��DhqDh��Di�Di�qDjqDj��Dk�Dk�qDl�Dl��Dm�Dm��DnqDn�qDo�Do��Dp=Dp�=DqqDq��Dr�Dr�qDs�Ds�=Dt=Dt�=Du=Du��DvqDv�qDwqDw�
Dw�Dy�D�AHD�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A�  A�  A�A�%A�1A�1A�
=A�JA�JA�JA�
=A�
=A�1A�A���AڼjAϸRA�=qAʧ�A�1Aƥ�A�=qAŬA�1A���A���A�hsA�+A��wA�;dA�p�A��A�"�A��A�ĜA�n�A��uA���A���A�$�A��A�p�A���A�ĜA�~�A�bA�C�A��FA�dZA��wA��PA��7A�9XA��A�x�A��hA��mA��A�9XA�  A�&�A��;A���A��A��A��7A���A���A���A��A��/A�(�A��^A�bA�1A�bNA���A�S�A�+A�XA�1A���A��A|�AxE�Av  AsK�Aq��AoS�An�\AmAjI�Ah=qAe�;AcS�Aa�7A`��A_��A]"�AZ�AY�;AXM�AWoAVATZAS/AOp�AMp�AL=qAKhsAI�AG�AG7LAE�FAD��ABA�A?\)A?A>(�A<��A:��A9��A8��A7oA5ƨA45?A2r�A1K�A0��A/��A.�yA.��A.jA-�hA,�\A+��A)?}A(r�A'�A$��A#��A#G�A"�RA!�mA ��A ��A E�A�A�A
=A�PA%A��AVA�A%A�A�A��A��A?}Ar�A/A%A�/A��A;dA
=AoAVA�A  A
�/A	�mA
^5A
Q�A
1'A	��A�jA�+A�^A�jAZA5?AƨA\)A��A��A�hA`BA�A33AAr�A|�A �HA ��A 1'@�J@��9@���@�%@�1@�&�@�b@�;d@���@�7L@�j@��m@��@�  @� �@���@�-@�@�ȴ@�J@�Z@��@���@�
=@�ff@݁@�t�@ڰ!@ٙ�@��@�Q�@���@�\)@��@Չ7@�j@ҟ�@��@�?}@��y@�^5@�X@˕�@��H@ʏ\@�E�@ʧ�@�-@ʧ�@�l�@ʗ�@�M�@ȋD@�ƨ@ư!@��@�V@��@�o@�{@�hs@�G�@�V@��`@���@��@�Q�@���@�;d@��R@��-@�%@���@�Q�@��@��@��F@��@���@�ȴ@�n�@�J@���@�`B@�I�@���@�@���@��-@�M�@��+@�M�@�M�@�=q@���@�?}@�%@��@�1'@���@�33@�
=@�~�@��@�p�@��@�Ĝ@�bN@��m@�l�@��H@���@�M�@��@��@�I�@��m@�l�@�K�@�;d@�
=@�ff@��@�G�@�G�@�Ĝ@�1'@��m@�ƨ@���@��@��+@���@�z�@�r�@��#@�7L@���@��@��u@�z�@�(�@�I�@�Q�@�1@��+@��-@�G�@��9@�r�@��u@���@�Q�@��;@�l�@�S�@�\)@�dZ@��@�@���@��T@���@��@��`@�Ĝ@�z�@�1@�  @��m@��
@� �@��u@�r�@��
@�C�@�|�@�;d@���@��!@��\@�5?@��@���@��T@��#@��7@�%@���@���@��@�j@�1'@�1@��F@�"�@���@�^5@�=q@��\@��\@�n�@�p�@��/@���@�Ĝ@��/@�&�@���@���@��@�j@���@�{@�J@���@�X@��@���@�j@�j@�Z@�Ĝ@��/@�bN@���@�ƨ@�;d@��H@��!@�n�@�M�@�{@�@���@��@��T@���@���@�G�@���@�j@�A�@�b@��;@���@��@�K�@��@�M�@�-@�@���@��7@�%@��D@�I�@�9X@��@��w@��P@�|�@�@��\@�=q@��#@��@�&�@��`@���@��u@�I�@�b@��m@��w@���@�l�@�;d@��y@�ȴ@���@��R@���@��+@�n�@���@�x�@�X@�G�@��@���@��9@�(�@�1@�@~��@m7L@[�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A�  A�  A�A�%A�1A�1A�
=A�JA�JA�JA�
=A�
=A�1A�A���AڼjAϸRA�=qAʧ�A�1Aƥ�A�=qAŬA�1A���A���A�hsA�+A��wA�;dA�p�A��A�"�A��A�ĜA�n�A��uA���A���A�$�A��A�p�A���A�ĜA�~�A�bA�C�A��FA�dZA��wA��PA��7A�9XA��A�x�A��hA��mA��A�9XA�  A�&�A��;A���A��A��A��7A���A���A���A��A��/A�(�A��^A�bA�1A�bNA���A�S�A�+A�XA�1A���A��A|�AxE�Av  AsK�Aq��AoS�An�\AmAjI�Ah=qAe�;AcS�Aa�7A`��A_��A]"�AZ�AY�;AXM�AWoAVATZAS/AOp�AMp�AL=qAKhsAI�AG�AG7LAE�FAD��ABA�A?\)A?A>(�A<��A:��A9��A8��A7oA5ƨA45?A2r�A1K�A0��A/��A.�yA.��A.jA-�hA,�\A+��A)?}A(r�A'�A$��A#��A#G�A"�RA!�mA ��A ��A E�A�A�A
=A�PA%A��AVA�A%A�A�A��A��A?}Ar�A/A%A�/A��A;dA
=AoAVA�A  A
�/A	�mA
^5A
Q�A
1'A	��A�jA�+A�^A�jAZA5?AƨA\)A��A��A�hA`BA�A33AAr�A|�A �HA ��A 1'@�J@��9@���@�%@�1@�&�@�b@�;d@���@�7L@�j@��m@��@�  @� �@���@�-@�@�ȴ@�J@�Z@��@���@�
=@�ff@݁@�t�@ڰ!@ٙ�@��@�Q�@���@�\)@��@Չ7@�j@ҟ�@��@�?}@��y@�^5@�X@˕�@��H@ʏ\@�E�@ʧ�@�-@ʧ�@�l�@ʗ�@�M�@ȋD@�ƨ@ư!@��@�V@��@�o@�{@�hs@�G�@�V@��`@���@��@�Q�@���@�;d@��R@��-@�%@���@�Q�@��@��@��F@��@���@�ȴ@�n�@�J@���@�`B@�I�@���@�@���@��-@�M�@��+@�M�@�M�@�=q@���@�?}@�%@��@�1'@���@�33@�
=@�~�@��@�p�@��@�Ĝ@�bN@��m@�l�@��H@���@�M�@��@��@�I�@��m@�l�@�K�@�;d@�
=@�ff@��@�G�@�G�@�Ĝ@�1'@��m@�ƨ@���@��@��+@���@�z�@�r�@��#@�7L@���@��@��u@�z�@�(�@�I�@�Q�@�1@��+@��-@�G�@��9@�r�@��u@���@�Q�@��;@�l�@�S�@�\)@�dZ@��@�@���@��T@���@��@��`@�Ĝ@�z�@�1@�  @��m@��
@� �@��u@�r�@��
@�C�@�|�@�;d@���@��!@��\@�5?@��@���@��T@��#@��7@�%@���@���@��@�j@�1'@�1@��F@�"�@���@�^5@�=q@��\@��\@�n�@�p�@��/@���@�Ĝ@��/@�&�@���@���@��@�j@���@�{@�J@���@�X@��@���@�j@�j@�Z@�Ĝ@��/@�bN@���@�ƨ@�;d@��H@��!@�n�@�M�@�{@�@���@��@��T@���@���@�G�@���@�j@�A�@�b@��;@���@��@�K�@��@�M�@�-@�@���@��7@�%@��D@�I�@�9X@��@��w@��P@�|�@�@��\@�=q@��#@��@�&�@��`@���@��u@�I�@�b@��m@��w@���@�l�@�;d@��y@�ȴ@���@��R@���@��+@�n�@���@�x�@�X@�G�@��@���@��9@�(�@�1@�@~��@m7L@[�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B49B33B33B33B33B33B33B49B49B.B49B33B33B49B49B49B33B2-B�B�B&�B9XBI�BR�BT�BVBk�Bq�Bv�B�7B�VB�=B�hB��B�oB�oB�{B�PB�=B�1B�B�B}�B{�By�Bt�BhsB_;BQ�BH�BB�B7LB0!B'�B�B�BhBB��B�B�mB�TB�/B��B�B�oB{�Bu�BhsB_;BM�B6FB0!B)�BoB
��B
�ZB
ȴB
�-B
��B
�+B
w�B
k�B
YB
R�B
G�B
2-B
�B
JB	��B	�B	�TB	�NB	�/B	��B	��B	�-B	��B	�uB	�JB	�B	r�B	e`B	]/B	S�B	L�B	E�B	<jB	2-B	"�B	�B	hB	JB	B��B��B�B�mB�/B�B�B�
B��B��B��B��BĜB�}B�^B�9B�'B�B�B�B�B�B��B��B��B��B��B��B�hB�bB�\B�\B�JB�=B�1B�%B�B~�B{�Bw�Bw�Bw�Bw�Bv�Bw�Bx�Bx�Bz�B|�B}�B� B�B�B�B� B� B�B�B�B�B�B�B}�B�=B�JB�=B�+B�B�B�B�B�%B�+B�%B�B�B�%B�+B�=B�7B�JB�VB�JB�DB�DB�DB�DB�PB�PB�DB�Bw�Bp�Bo�Bp�Bt�Bv�Bs�Bu�Bz�B�B�1B�JB�JB�=B�\B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�RBĜBȴBƨB��B��B��B��B��B��B�HB�sB�B�B�B�B�B�B�B�B�B��B��B��B��B	B	B	B	B	B	1B	1B		7B		7B	
=B	DB	PB	
=B		7B	DB	VB	uB	�B	�B	�B	�B	#�B	'�B	(�B	+B	.B	1'B	49B	49B	7LB	;dB	=qB	@�B	A�B	C�B	E�B	H�B	L�B	N�B	P�B	XB	]/B	^5B	aHB	dZB	e`B	e`B	ffB	iyB	l�B	l�B	m�B	o�B	q�B	q�B	r�B	u�B	t�B	r�B	q�B	p�B	u�B	~�B	~�B	~�B	�B	�B	�B	�B	�%B	�+B	�+B	�%B	�%B	�%B	�1B	�1B	�JB	�VB	�VB	�VB	�VB	�\B	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�'B	�-B	�9B	�LB	�RB	�dB	�jB	��B	ÖB	ĜB	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�)B	�)B	�#B	�#B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�)B	�)B	�/B	�5B	�;B	�BB	�HB	�NB	�TB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�`B	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�0B
�B
$@2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B49B33B33B33B33B33B33B49B49B.B49B33B33B49B49B49B33B2-B�B�B&�B9XBI�BR�BT�BVBk�Bq�Bv�B�7B�VB�=B�hB��B�oB�oB�{B�PB�=B�1B�B�B}�B{�By�Bt�BhsB_;BQ�BH�BB�B7LB0!B'�B�B�BhBB��B�B�mB�TB�/B��B�B�oB{�Bu�BhsB_;BM�B6FB0!B)�BoB
��B
�ZB
ȴB
�-B
��B
�+B
w�B
k�B
YB
R�B
G�B
2-B
�B
JB	��B	�B	�TB	�NB	�/B	��B	��B	�-B	��B	�uB	�JB	�B	r�B	e`B	]/B	S�B	L�B	E�B	<jB	2-B	"�B	�B	hB	JB	B��B��B�B�mB�/B�B�B�
B��B��B��B��BĜB�}B�^B�9B�'B�B�B�B�B�B��B��B��B��B��B��B�hB�bB�\B�\B�JB�=B�1B�%B�B~�B{�Bw�Bw�Bw�Bw�Bv�Bw�Bx�Bx�Bz�B|�B}�B� B�B�B�B� B� B�B�B�B�B�B�B}�B�=B�JB�=B�+B�B�B�B�B�%B�+B�%B�B�B�%B�+B�=B�7B�JB�VB�JB�DB�DB�DB�DB�PB�PB�DB�Bw�Bp�Bo�Bp�Bt�Bv�Bs�Bu�Bz�B�B�1B�JB�JB�=B�\B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�RBĜBȴBƨB��B��B��B��B��B��B�HB�sB�B�B�B�B�B�B�B�B�B��B��B��B��B	B	B	B	B	B	1B	1B		7B		7B	
=B	DB	PB	
=B		7B	DB	VB	uB	�B	�B	�B	�B	#�B	'�B	(�B	+B	.B	1'B	49B	49B	7LB	;dB	=qB	@�B	A�B	C�B	E�B	H�B	L�B	N�B	P�B	XB	]/B	^5B	aHB	dZB	e`B	e`B	ffB	iyB	l�B	l�B	m�B	o�B	q�B	q�B	r�B	u�B	t�B	r�B	q�B	p�B	u�B	~�B	~�B	~�B	�B	�B	�B	�B	�%B	�+B	�+B	�%B	�%B	�%B	�1B	�1B	�JB	�VB	�VB	�VB	�VB	�\B	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�'B	�-B	�9B	�LB	�RB	�dB	�jB	��B	ÖB	ĜB	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�)B	�)B	�#B	�#B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�)B	�)B	�/B	�5B	�;B	�BB	�HB	�NB	�TB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�`B	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�0B
�B
$@2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191714                              AO  ARCAADJP                                                                    20181005191714    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191714  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191714  QCF$                G�O�G�O�G�O�8000            