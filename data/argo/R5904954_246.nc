CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:45Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005191745  20181005191745  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @���a)51   @���#`@5�����d��1&�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC�fC�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CU�fCX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Cs�fCu�fCx  Cz  C{�fC}�fC�  C�  C�  C��3C�  C��3C��3C��3C��3C�  C�  C��3C��3C��3C�  C��C�  C�  C�  C��3C��C��C�  C��C��C��C��C�  C��3C�  C�  C��3C��3C��3C�  C��C�  C�  C��C��C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��C��C�  C��C��C�  C��3C��3C��3C��3C�  C�  C�  C��3C�  C��C��C��C�  C��3C��3C��C�  C�  C�  C��3C��3C��3C��C��3C��C��C�  C�  C�  C�  C��C�  C�  C��3C��3C�  C��C�  C��3C��3C��3C�  C�  C�  C�  C��3C��C��3C�  C�  C��C��3C�  C��C��C��C��C�  C��3C��3C�  D   D � DfD�fDfDy�D  D� D��D� D  D� D  D� D��D�fDfDs3D	  D	� D
fD
� D
��D� D  D� D��D� DfD� D  D� D  D� D  Dy�DfDy�D  D� D  D� D  D� DfD�fD  Dy�D  D� D��Dy�D  D� D  D�fD  D� D  D� DfD� DfD� D   D � D!  D!y�D!��D"�fD"��D#� D$  D$y�D%  D%� D%��D&� D'fD'� D(fD(� D(��D)y�D)��D*�fD+  D+�fD,  D,� D,��D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D3��D4�fD5  D5� D6�D6�fD7  D7� D8  D8y�D9fD9�fD:  D:� D;  D;� D<  D<� D=  D=�fD=��D>� D?  D?� D@  D@� D@��DA� DB  DB� DC  DC� DD  DD� DD��DE� DF  DF� DG  DG� DH  DH� DI  DIy�DJfDJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DRfDRy�DS  DS� DT  DT� DU  DU� DV  DV�fDW  DW� DX  DXy�DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^� D_  D_�fD`  D`� DafDa� Db  Db� Db��Dc� Dd  Dd� De  De� Df  Df� Df��Dg�fDh  Dh� DifDi� Dj�Dj� Dk  Dk�fDl  Dl� Dl��Dm�fDm��Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dv��Dw� DxfDxFfDy��D�+3D��{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��]@���Az�A$z�ADz�Adz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��\B��\B��\B�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\Bď\Bȏ\B�\)BЏ\Bԏ\B؏\B܏\B��\B�\B�\B�\B��\B�\B��\B��\C G�CG�C.C.C.C
G�CG�CG�CG�CG�CG�CG�CG�CG�CG�CG�C G�C"G�C$G�C&.C(G�C*G�C,G�C.G�C0G�C2G�C4G�C6G�C8G�C:.C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CNG�CP.CRG�CTG�CV.CXG�CZG�C\G�C^G�C`aHCbG�CdG�CfG�ChG�CjG�ClaHCnG�CpG�CrG�Ct.Cv.CxG�CzG�C|.C~.C�#�C�#�C�#�C�
C�#�C�
C�
C�
C�
C�#�C�#�C�
C�
C�
C�#�C�0�C�#�C�#�C�#�C�
C�0�C�0�C�#�C�0�C�0�C�0�C�0�C�#�C�
C�#�C�#�C�
C�
C�
C�#�C�0�C�#�C�#�C�0�C�0�C�#�C�#�C�#�C�#�C�0�C�0�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�0�C�0�C�#�C�0�C�0�C�#�C�
C�
C�
C�
C�#�C�#�C�#�C�
C�#�C�0�C�0�C�0�C�#�C�
C�
C�0�C�#�C�#�C�#�C�
C�
C�
C�0�C�
C�0�C�=qC�#�C�#�C�#�C�#�C�0�C�#�C�#�C�
C�
C�#�C�0�C�#�C�
C�
C�
C�#�C�#�C�#�C�#�C�
C�0�C�
C�#�C�#�C�0�C�
C�#�C�0�C�0�C�0�C�0�C�#�C�
C�
C�#�D �D ��DRD�RDRD��D�D��D�D��D�D��D�D��D�D�RDRD�D	�D	��D
RD
��D�D��D�D��D�D��DRD��D�D��D�D��D�D��DRD��D�D��D�D��D�D��DRD�RD�D��D�D��D�D��D�D��D�D�RD�D��D�D��DRD��DRD��D �D ��D!�D!��D"�D"�RD#�D#��D$�D$��D%�D%��D&�D&��D'RD'��D(RD(��D)�D)��D*�D*�RD+�D+�RD,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4�RD5�D5��D6�D6�RD7�D7��D8�D8��D9RD9�RD:�D:��D;�D;��D<�D<��D=�D=�RD>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJRDJ��DKRDK��DL�DL��DM�DM��DN�DN��DO�DO��DPRDP��DQ�DQ��DRRDR��DS�DS��DT�DT��DU�DU��DV�DV�RDW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^RD^��D_�D_�RD`�D`��DaRDa��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg�RDh�Dh��DiRDi��Dj�Dj��Dk�Dk�RDl�Dl��Dm�Dm�RDn�Dn�RDo�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��DxRDxXRDy�qD�4)D��q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�M�A�K�A�I�A�A�A�?}A�G�A�7LA�7LAȧ�A�hsA��AǶFAǝ�AǃAƼjAŰ!A�A�A�{A�A��A��A��A��`A��TA��#Aě�A�Q�A�E�A�9XA��A��A�ƨA�M�A��;ADA�Q�A��yA��A� �A�`BA�1A��mA��yA��A��A��A���A��DA�jA���A�9XA��wA�bNA�VA�Q�A��mA���A���A���A�~�A�{A��A���A�ffA�A�ƨA�jA�  A�1A�VA���A��A�{A�hsA��A���A�jA�I�A���A�=qA���A��jA�S�A�?}A���A�^5A���A�jA��wA��A���A��!A��^A��TA�?}A���A�Q�A��-A��A�ȴA��7A��yA|�A|�jA{�Az�RAydZAw�mAvjAt�`At=qAs��Ar��Ao��AmO�Ahv�Af5?AdI�Ab=qAaVA_�PA\��AZ�9AZ1AY7LAX��AX  AW%AVr�AU�AR�!AO�AN(�ALI�AJ$�AIoAGx�AF��AF1AEC�AC�-AB$�A?�TA=��A<�HA<v�A;
=A9��A8�DA7��A6n�A6bA5C�A2bNA/��A-l�A,ffA,5?A,  A+��A+x�A*JA'�;A'7LA&��A&n�A%��A$ZA#7LA"��A"ZA!O�A (�A�HA�AffA;dAI�A��A�mA?}A�A�TA�A�yA{A^5A�;A�HA^5A  A%A�Ar�A&�A��A
��A
bNA	�AA�AZA"�A�
AS�A�@��@���@��@��!@�9X@�7L@�"�@�@��@�Ĝ@�@웦@��@�"�@�@��@ꗍ@���@�-@�X@��@��
@�=q@�$�@���@��
@�+@���@ݑh@�C�@�E�@��#@٩�@�O�@ו�@ա�@��@Լj@�j@�S�@�M�@Ѻ^@���@�33@�V@���@�p�@̣�@�$�@�`B@��;@�^5@Ɵ�@�ȴ@�v�@���@��j@�ƨ@���@�33@���@�`B@�ƨ@���@��w@���@�{@�G�@���@���@���@��@�+@��@�=q@���@��@���@�G�@��@�7L@�V@���@��
@��@�33@��y@��!@���@��\@��\@���@��\@�v�@�V@�V@�~�@��+@��\@�v�@�^5@�^5@�^5@�V@�E�@�E�@�=q@�=q@�E�@�E�@�E�@�$�@�@��@��9@�bN@��@��;@��@�l�@�~�@���@��j@�Z@�(�@��@���@���@�~�@��@���@��@��`@��D@�A�@��;@��@���@�V@��@���@��h@�p�@��@�j@�I�@���@�|�@�"�@��\@��-@���@���@��@��;@�ƨ@��@�\)@�V@�@��@���@�?}@���@���@��@���@��/@���@���@��9@��u@��@�bN@�9X@�b@��;@��F@�S�@�;d@��y@��+@�ff@�v�@��+@��T@��@���@�
=@�ȴ@�-@���@���@�x�@�7L@�G�@�O�@���@��9@���@�Q�@��@�ƨ@�C�@��@���@��@�ȴ@��R@���@��+@�~�@�M�@��@���@�@���@��7@�p�@�O�@�/@��@��/@��j@��D@�z�@�bN@�bN@�Q�@�9X@�  @���@�C�@�ȴ@��R@���@���@�ff@���@���@��@�7L@���@��/@�Ĝ@��9@��u@�Q�@�(�@��@�b@��
@���@��@�dZ@�K�@�;d@�+@��y@��\@��@���@���@���@��7@��@�7L@�Ĝ@�bN@���@�ƨ@�K�@��@�ȴ@���@�ff@�ff@�V@�E�@�J@���@���@���@�J@���@���@�7L@��@�Ĝ@��`@�(�@��@s��@c9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�M�A�K�A�I�A�A�A�?}A�G�A�7LA�7LAȧ�A�hsA��AǶFAǝ�AǃAƼjAŰ!A�A�A�{A�A��A��A��A��`A��TA��#Aě�A�Q�A�E�A�9XA��A��A�ƨA�M�A��;ADA�Q�A��yA��A� �A�`BA�1A��mA��yA��A��A��A���A��DA�jA���A�9XA��wA�bNA�VA�Q�A��mA���A���A���A�~�A�{A��A���A�ffA�A�ƨA�jA�  A�1A�VA���A��A�{A�hsA��A���A�jA�I�A���A�=qA���A��jA�S�A�?}A���A�^5A���A�jA��wA��A���A��!A��^A��TA�?}A���A�Q�A��-A��A�ȴA��7A��yA|�A|�jA{�Az�RAydZAw�mAvjAt�`At=qAs��Ar��Ao��AmO�Ahv�Af5?AdI�Ab=qAaVA_�PA\��AZ�9AZ1AY7LAX��AX  AW%AVr�AU�AR�!AO�AN(�ALI�AJ$�AIoAGx�AF��AF1AEC�AC�-AB$�A?�TA=��A<�HA<v�A;
=A9��A8�DA7��A6n�A6bA5C�A2bNA/��A-l�A,ffA,5?A,  A+��A+x�A*JA'�;A'7LA&��A&n�A%��A$ZA#7LA"��A"ZA!O�A (�A�HA�AffA;dAI�A��A�mA?}A�A�TA�A�yA{A^5A�;A�HA^5A  A%A�Ar�A&�A��A
��A
bNA	�AA�AZA"�A�
AS�A�@��@���@��@��!@�9X@�7L@�"�@�@��@�Ĝ@�@웦@��@�"�@�@��@ꗍ@���@�-@�X@��@��
@�=q@�$�@���@��
@�+@���@ݑh@�C�@�E�@��#@٩�@�O�@ו�@ա�@��@Լj@�j@�S�@�M�@Ѻ^@���@�33@�V@���@�p�@̣�@�$�@�`B@��;@�^5@Ɵ�@�ȴ@�v�@���@��j@�ƨ@���@�33@���@�`B@�ƨ@���@��w@���@�{@�G�@���@���@���@��@�+@��@�=q@���@��@���@�G�@��@�7L@�V@���@��
@��@�33@��y@��!@���@��\@��\@���@��\@�v�@�V@�V@�~�@��+@��\@�v�@�^5@�^5@�^5@�V@�E�@�E�@�=q@�=q@�E�@�E�@�E�@�$�@�@��@��9@�bN@��@��;@��@�l�@�~�@���@��j@�Z@�(�@��@���@���@�~�@��@���@��@��`@��D@�A�@��;@��@���@�V@��@���@��h@�p�@��@�j@�I�@���@�|�@�"�@��\@��-@���@���@��@��;@�ƨ@��@�\)@�V@�@��@���@�?}@���@���@��@���@��/@���@���@��9@��u@��@�bN@�9X@�b@��;@��F@�S�@�;d@��y@��+@�ff@�v�@��+@��T@��@���@�
=@�ȴ@�-@���@���@�x�@�7L@�G�@�O�@���@��9@���@�Q�@��@�ƨ@�C�@��@���@��@�ȴ@��R@���@��+@�~�@�M�@��@���@�@���@��7@�p�@�O�@�/@��@��/@��j@��D@�z�@�bN@�bN@�Q�@�9X@�  @���@�C�@�ȴ@��R@���@���@�ff@���@���@��@�7L@���@��/@�Ĝ@��9@��u@�Q�@�(�@��@�b@��
@���@��@�dZ@�K�@�;d@�+@��y@��\@��@���@���@���@��7@��@�7L@�Ĝ@�bN@���@�ƨ@�K�@��@�ȴ@���@�ff@�ff@�V@�E�@�J@���@���@���@�J@���@���@�7L@��@�Ĝ@��`@�(�@��@s��@c9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�qB
�qB
�qB
�dB
�dB
�qB
�dB
��B>wBM�B^5Bv�B~�B�7B��B�B7LB>wB@�BC�BD�BD�BF�BF�BH�BS�B\)B]/B^5BaHBgmBp�B�B�bB��B��B�B�FB�}BƨB��B�;B�ZB�B�mB�B�B�B�TB��B��B�qB�dB�dB�^B�RB�?B�RB�jB�3B��B�uB�=B�Bq�BR�BC�B2-B#�B�B{BVB+BB��B�#B��B��BȴB�wB�B��B��B��B�uB�Bp�BjB^5BA�B �BJB
��B
�B
�mB
�BB
�
B
��B
�-B
�{B
�B
t�B
bNB
H�B
8RB
6FB
1'B
+B
(�B
$�B
"�B
 �B
�B
B	�B	�BB	��B	ĜB	�RB	�!B	��B	�{B	�1B	�B	� B	{�B	v�B	q�B	m�B	ffB	VB	F�B	A�B	7LB	.B	&�B	 �B	�B	�B	{B	VB	%B��B��B��B�B�B�fB�NB�;B�#B�B��B��B��B�dB�LB�LB�?B�9B�-B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�VB�JB�7B�+B�+B�+B�1B�+B�+B�%B�B�B�B� B� B~�B}�B}�B|�B}�B}�B� B�B� B�B�B� B� B}�B{�B{�B{�Bz�Bz�Bx�By�By�By�By�By�B{�B~�B�B�B�B�B�B�B�B�B�B�B�%B�bB�hB�oB�{B�uB��B��B��B��B��B��B��B��B�{B�uB�oB��B��B��B��B��B��B��B��B��B�oB�oB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�!B�-B�9B�3B�3B�?B�^B�}B��BÖBǮBȴBɺB��B��B��B�
B�B�B�;B�BB�HB�TB�yB�B�B��B��B��B	B	B	B	B	B	B	B	B	B	%B	
=B	hB	oB	uB	{B	�B	�B	�B	&�B	,B	1'B	5?B	7LB	;dB	>wB	>wB	?}B	A�B	B�B	F�B	H�B	J�B	K�B	L�B	R�B	T�B	W
B	XB	ZB	[#B	\)B	^5B	_;B	aHB	e`B	ffB	e`B	e`B	e`B	iyB	l�B	l�B	k�B	l�B	p�B	q�B	r�B	u�B	x�B	{�B	{�B	|�B	�B	�B	�B	�1B	�\B	�oB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�3B	�3B	�?B	�FB	�LB	�RB	�RB	�XB	�XB	�^B	�dB	�jB	�wB	��B	��B	��B	B	B	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�5B	�;B	�BB	�HB	�NB	�TB	�TB	�NB	�TB	�ZB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
\B	��B
B
($2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
�qB
�qB
�qB
�dB
�dB
�qB
�dB
��B>wBM�B^5Bv�B~�B�7B��B�B7LB>wB@�BC�BD�BD�BF�BF�BH�BS�B\)B]/B^5BaHBgmBp�B�B�bB��B��B�B�FB�}BƨB��B�;B�ZB�B�mB�B�B�B�TB��B��B�qB�dB�dB�^B�RB�?B�RB�jB�3B��B�uB�=B�Bq�BR�BC�B2-B#�B�B{BVB+BB��B�#B��B��BȴB�wB�B��B��B��B�uB�Bp�BjB^5BA�B �BJB
��B
�B
�mB
�BB
�
B
��B
�-B
�{B
�B
t�B
bNB
H�B
8RB
6FB
1'B
+B
(�B
$�B
"�B
 �B
�B
B	�B	�BB	��B	ĜB	�RB	�!B	��B	�{B	�1B	�B	� B	{�B	v�B	q�B	m�B	ffB	VB	F�B	A�B	7LB	.B	&�B	 �B	�B	�B	{B	VB	%B��B��B��B�B�B�fB�NB�;B�#B�B��B��B��B�dB�LB�LB�?B�9B�-B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�VB�JB�7B�+B�+B�+B�1B�+B�+B�%B�B�B�B� B� B~�B}�B}�B|�B}�B}�B� B�B� B�B�B� B� B}�B{�B{�B{�Bz�Bz�Bx�By�By�By�By�By�B{�B~�B�B�B�B�B�B�B�B�B�B�B�%B�bB�hB�oB�{B�uB��B��B��B��B��B��B��B��B�{B�uB�oB��B��B��B��B��B��B��B��B��B�oB�oB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�!B�-B�9B�3B�3B�?B�^B�}B��BÖBǮBȴBɺB��B��B��B�
B�B�B�;B�BB�HB�TB�yB�B�B��B��B��B	B	B	B	B	B	B	B	B	B	%B	
=B	hB	oB	uB	{B	�B	�B	�B	&�B	,B	1'B	5?B	7LB	;dB	>wB	>wB	?}B	A�B	B�B	F�B	H�B	J�B	K�B	L�B	R�B	T�B	W
B	XB	ZB	[#B	\)B	^5B	_;B	aHB	e`B	ffB	e`B	e`B	e`B	iyB	l�B	l�B	k�B	l�B	p�B	q�B	r�B	u�B	x�B	{�B	{�B	|�B	�B	�B	�B	�1B	�\B	�oB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�3B	�3B	�?B	�FB	�LB	�RB	�RB	�XB	�XB	�^B	�dB	�jB	�wB	��B	��B	��B	B	B	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�5B	�;B	�BB	�HB	�NB	�TB	�TB	�NB	�TB	�ZB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
\B	��B
B
($2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191745                              AO  ARCAADJP                                                                    20181005191745    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191745  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191745  QCF$                G�O�G�O�G�O�8000            