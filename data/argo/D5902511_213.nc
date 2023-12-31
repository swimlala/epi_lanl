CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-05-17T17:09:30Z creation; 2023-02-10T23:09:44Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     (  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \0   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     (  c�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     (  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     (  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     (  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     (  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ( (   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 6P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ( >   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � ]D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ( e   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220517170930  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_213                 6810_008521_213                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�Ч��+k@�Ч��+k11  @�ЧƧ�@�ЧƧ�@0U-#N��@0U-#N���d�MUi�d�MUi11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�@B�\@�  @�  @\@�G�@�(�A\)A   A,��AAG�AaG�A�Q�A��A�Q�A�Q�A�Q�A�  A߮A�A��B�
B  B(�B (�B(  B/�
B7�
B@  BG�
BO�
BX  B`(�BhQ�BpQ�Bx  B�
B��
B��B�(�B�  B�  B�{B�  B�  B��B�  B�{B��B�  B�{B�{B��B��B�  B��B�  B�{B�  B�  B�{B�  B�  B�  B�(�B�  B��
B�{C   C  C  C��C��C
  C  C  C
=C
=C  C��C  C
=C  C  C   C"  C#��C%��C'�C*  C,
=C.  C0  C2  C4
=C5��C8  C:  C<  C>  C@  CA��CC��CF  CH  CJ  CL
=CM��CP
=CR  CT  CV  CW��CY��C\  C^
=C`
=Cb  Cd
=Cf  Cg��Ci��Cl  Cn
=Cp  Cr
=Cs��Cv
=Cx{Cz�C|{C~  C�  C���C���C�  C�C�  C���C���C�  C�C�
=C�C���C���C�C�C�C�  C�C�C�C�
=C�C���C���C�C�C�  C�  C�C���C���C�  C���C�  C�
=C�C�  C�C�C���C���C���C�  C�  C���C�C�  C���C�  C�C�C�  C�C�
=C�  C���C���C���C�  C�  C���C�C�C���C�  C�
=C�C�  C���C�  C���C���C���C�  C�  C�C�  C���C�  C�  C�  C�C�C�C�C���C���C���C���C�  C���C�  C�  C�C�  C�  C�
=C�C�  C���C���C�  C�  C�C�C�C�C�  C�  C�C�
=C���C���C�C�\C�
=C�C�C�
=C�  C���C���C���C�C�C���C���C���D }qD �qD}qD�qD� D��D}qD  D� D  D� D�D� D  D�DD��D	  D	� D
�D
� D  D��D�D��D�qDz�D  D�D�D}qD�qD� D�D��D�D�D�qD}qD�qD� D�qDz�D��Dz�D�qDz�D�qD� D  D��D  Dz�D�qD��D  D}qD�D��D�qD� D�D� D   D � D!�D!��D"  D"��D#�D#� D$  D$��D%�D%}qD%�qD&��D'�D'��D(�D(}qD)  D)��D*  D*��D+  D+}qD,D,�D-�D-��D-�qD.� D/�D/}qD/�qD0� D1  D1}qD1��D2}qD2��D3z�D4  D4��D5�D5��D6D6}qD6��D7}qD7�qD8��D9D9}qD9�qD:}qD;  D;��D<  D<}qD<�qD=� D>  D>�D?D?��D?��D@� DADA}qDB  DB��DC  DC}qDD  DD� DE  DE� DF  DF� DG�DG� DH  DH��DIDI��DJ�DJ��DK�DK}qDL  DL��DM  DM}qDM��DNz�DN��DO}qDP  DP��DQ�DQ� DR  DR��DS  DS� DT  DT� DU�DU�DVDV��DW  DW��DX  DX� DY  DY� DZ  DZ� DZ�qD[}qD\�D\}qD]  D]��D^�D^��D^�qD_}qD_�qD`� D`�qDa}qDb  Db��Db�qDc� Dd  Ddz�Dd��De}qDe�qDf� Dg  Dg��DhDh�Di  Di}qDj  Dj� Dj�qDk}qDl  Dl��Dm  Dm}qDn  Dn}qDn��Do}qDp  Dp� Dq  Dq� Dq�qDr}qDs  Ds��Dt  Dt�DuDu� Du�qDv��Dw�Dw}qDw�qDx� Dy  Dy��Dz�Dz��D{D{� D|  D|}qD|�qD}� D~D~��D�D��D�  D�AHD���D��HD�HD�B�D���D��HD���D�@ D��HD��HD��D�B�D��HD��HD�HD�AHD�~�D���D�HD�@ D�� D��HD�  D�@ D��HD��HD�HD�@ D��HD��HD�HD�@ D�~�D�� D�  D�=qD�}qD���D���D�>�D�~�D�� D���D�AHD���D�� D�  D�AHD��HD�� D���D�@ D�� D��HD�  D�@ D��HD�� D�HD�AHD�~�D���D���D�@ D��HD��HD�HD�@ D�� D�� D���D�>�D�� D�D�  D�>�D�� D��HD�  D�>�D�}qD���D�  D�@ D�~�D���D�  D�AHD��HD���D�  D�@ D�� D�� D���D�AHD��HD���D�  D�@ D�~�D��HD�  D�@ D��HD��HD�HD�>�D�}qD��)D��qD�>�D�~�D�� D�  D�<)D�}qD���D���D�@ D�� D�� D�HD�@ D�� D��HD�  D�@ D�~�D���D�  D�>�D�� D��HD�HD�>�D�~�D���D��qD�@ D�� D�� D�  D�@ D�~�D���D�  D�>�D�� D�� D���D�@ D�~�D�� D�HD�@ D�}qD���D�  D�@ D�~�D��qD�  D�@ D��HD��HD�HD�AHD��HD��HD�HD�>�D�}qD�� D��D�@ D�� D���D�  D�B�D��HD�� D�HD�@ D�}qD���D���D�>�D�� D�� D�  D�AHD�� D�� D�  D�AHD�� D���D�  D�@ D�~�D�� D��D�AHD��HD��HD�  D�B�D��HD�� D�  D�>�D�� D��HD��D�B�D�� D���D�  D�AHD�~�D�� D���D�>�D�~�D���D�  D�>�D�~�D���D���D�>�D�� D�� D�  D�@ D�~�D���D�  D�@ D�~�D�� D��D�AHD��HD�D��D�B�D��HD���D�HD�B�D�� D�� D�HD�@ D�~�D��HD��D�>�D�~�D�� D���D�@ D D¾�D�  D�@ DÀ Dþ�D�HD�AHDāHD��HD�  D�@ D�~�DŽqD���D�B�DƁHD�� D�  D�>�D�}qD�� D�  D�@ DȀ DȾ�D��qD�=qD�}qDɾ�D�  D�>�D�~�D��HD�HD�@ D�~�D˽qD��qD�@ D́HD��HD�HD�B�D̀ D;�D���D�=qD�}qDξ�D�  D�>�D�~�D�� D�HD�B�DЁHD��HD�  D�@ DсHD�� D�  D�AHDҁHD�� D���D�>�DӀ D�� D�  D�@ DԁHD�D�HD�@ D�~�DսqD���D�@ DցHD�� D�  D�@ D�~�D׾�D��qD�@ D؁HD�� D�HD�AHD�~�DٽqD��qD�@ DځHDھ�D���D�AHDہHD�� D��D�AHD�~�DܽqD�  D�B�D݁HD��HD�HD�@ D�~�D�D��D�AHD߀ D߾�D���D�>�D�}qDྸD�HD�AHD�}qD�qD��qD�>�D� D�� D�  D�@ D� D�� D���D�>�D�~�D�� D�  D�@ D� D�� D���D�>�D� D�� D�HD�@ D� D�� D�  D�>�D�|)D辸D�HD�@ D�HD���D��D�@ D�~�D�D��D�AHD�~�D�� D�HD�AHD�~�D쾸D���D�>�D� D��HD�HD�@ D�~�D�� D�  D�@ D� D�� D�  D�@ D�� D��HD��D�AHD� D��HD�HD�@ D�HD�D�  D�AHD�HD�D�HD�B�D� D���D���D�>�D�� D���D���D�=qD�~�D�� D�HD�>�D��HD��)>�G�?�?L��?�z�?�Q�?���?��H@�@�R@5@G�@W
=@p��@��
@��@�
=@��@���@���@�ff@�\)@��H@���@��@��HAz�A
=qA{Az�A��A{A%�A)��A-p�A333A9��A=p�AA�AHQ�AL��AQG�AW�A\(�AaG�Ag
=Aj�HAo\)AuAy��A~{A��A�33A�{A�Q�A��A�(�A�
=A���A��\A�{A��A��A��A��RA�G�A�(�A��RA�Q�A��HA�{A�  A���A���A�
=A���A��
A�{A�Q�A��A���AǮA�G�A�(�A�\)A�G�A�33AָRA���A��HA�A��A��HA�A��A�\A��A�  A��A�z�A��A�=qA�(�A�\)B ��B�B\)B��BB�HBz�B	B
�\BQ�BB�RB�
B��BffB�BG�B=qB�B��B=qB33Bz�B{B
=B (�B!�B#
=B$(�B%��B'
=B'�
B)�B*�RB+�
B,��B.ffB/�B0��B1�B3�B4��B5B733B8��B9�B:�HB<Q�B=�B>�HB@  BABB�HBD  BE��BG
=BG�
BIG�BJ�HBL(�BM�BNffBP  BP��BR{BS�BT��BU�BW33BX��BY�BZ�HB\(�B]�B^�RB`  Ba��Bb�RBc�
BeG�Bf�\Bg�Bi�Bj�\Bk�Bl��BnffBo33Bpz�Br{Bs33Bt(�BuBw33Bx(�Byp�B{
=B|Q�B}G�B
=B�{B���B�\)B�(�B���B�\)B�{B���B�G�B�{B��RB�33B��B��RB�33B��
B���B�33B��B�z�B�33B�B�Q�B��B��
B�Q�B��HB�B�ffB��HB��B�Q�B��B���B�Q�B��B��B�Q�B��B��B�z�B�
=B��
B��\B�33B�B��\B�G�B�B��\B�G�B�B�Q�B�33B��
B�Q�B��B��
B�ffB���B�B�z�B�
=B���B�Q�B�
=B���B�{B���B���B�(�B��RB��B�(�B���B�G�B�  B��RB��B��B�Q�B���B�p�B��B��\B��B�p�B��B�z�B��HB�33B�B�=qB���B��HB�33B�B�  B�(�B���B���B��B�\)B�B�{B�=qB�z�B�
=B�\)B���B�B�=qB��\B���B�
=B�p�B��B�{B�=qB£�B��B�G�B�p�B��
B�=qB�ffBĸRB��B�\)Bř�B��B�ffBƏ\B��HB�\)B�B�  B�=qBȣ�B�
=BɅB��
B�  B�ffB��HB�G�B˅B�B�=qḄ�B���B�33BͅB�{B�ffBθRB��HB�G�B�B�{B�Q�BЏ\B��BхB�B��B�ffB��HB�33B�p�B�B�=qBԸRB�
=B�G�Bՙ�B�{B֏\B���B�G�BׅB��B�ffBظRB�
=B�G�B��
B�=qB�z�B���B�33BۮB�  B�=qBܣ�B��B݅BݮB�  B�z�B��HB�
=B�\)B��B�Q�B��\B�RB��BᙚB�  B�Q�B�\B���B�G�B�B�{B�Q�B�\B���B�p�B�B�  B�=qB�RB��B�p�B癚B�{B�z�B�RB��HB�p�B�B�{B�Q�B�\B�
=B�p�B�B��B�(�B�\B�
=B�G�B�B��B�{B�\B��HB��B�\)B��
B�=qB�z�B�RB�
=B�B��
B�  B�ffB���B�
=B�G�B�B�  B�Q�B�\B��RB��B�p�B��
B�(�B�Q�B��\B���B�\)B��B��B�  B�ffB��HB�33B�p�B��B�(�B���B��HB��B���B�{B�Q�B���B�
=B��B�B�  B�z�B���B�33B��B��
C �C \)C �C ��C �
C{C33CQ�C�CC�C  C33Cp�C��CC�HC
=CG�C�C��C�RC��C33CQ�Cp�C��C�C
=C(�CQ�C��C�RC�
C  C=qCp�C�C�C�C{C(�CQ�C�\C�RC��C�C(�CQ�Cz�C�\C�C�HC	{C	(�C	G�C	�C	�C	�RC	�C
�C
=qC
Q�C
z�C
�RC
�
C
�C�CG�CQ�Cz�C�RC�HC��C{CQ�Cz�C�C�RC��C  C(�C\)C�C��C��C
=C�CG�Cz�C�C�
C�C�C\)Cz�C�\C��C  C(�C=qCffC��C��C�C
=C33Cp�C��C�C�
C{C33CQ�C�C�RC��C��C33CQ�CffC��C�
C��C
=CG�Cz�C��C�RC�C(�C=qC\)C�\C�RC�C{C(�CQ�Cz�C�C�HC{C33CG�Cz�C�RC�C  C(�C\)C�\C�RC��C
=CG�Cz�C��CC�C(�CffC�\C��C�HC�CQ�Cp�C�\C�
C
=C33CQ�C��C��C��C{CQ�C��C��C��C�C\)C��C��C��C(�Cp�C�C�RC�C 33C ffC �\C �RC ��C!33C!\)C!z�C!�C!��C"33C"\)C"z�C"�RC#  C#=qC#ffC#�\C#��C${C$G�C$p�C$��C$�
C%�C%ffC%��C%�RC%��C&=qC&�C&C&�C'�C'Q�C'�C'C(
=C(=qC(p�C(��C(�
C)�C)Q�C)z�C)�RC)��C*G�C*p�C*��C*�
C+{C+ffC+��C+C,
=C,Q�C,�\C,�RC,�C-(�C-z�C-�RC-�C.�C.G�C.�C.��C/
=C/G�C/z�C/��C/�HC0(�C0ffC0��C0��C0��C133C1z�C1�RC1�C2�C2G�C2�C2�
C3{C3G�C3�C3�RC3�
C4{C4\)C4��C4��C4��C5�C5ffC5�C5�HC6
=C633C6p�C6��C6�HC7�C7\)C7��C7��C7��C8(�C8z�C8�RC8�HC9
=C9=qC9z�C9C9�C:{C:=qC:p�C:C;  C;(�C;Q�C;�C;�C;�
C<{C<Q�C<�\C<C<�HC=
=C==qC=z�C=�RC=�HC>
=C>=qC>p�C>��C>�HC?{C?G�C?z�C?�C?�HC@
=C@=qC@\)C@�C@CA  CA33CAffCA�\CACA�CB
=CB=qCBffCB��CB�
CC
=CC=qCCp�CC��CC��CD
=CD=qCDffCD��CD�
CE  CE33CEp�CE��CE��CE��CF33CFffCF��CF��CF��CG33CGffCG��CGCH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                 ?��@�@B�\@�  @�  @\@�G�@�(�A\)A   A,��AAG�AaG�A�Q�A��A�Q�A�Q�A�Q�A�  A߮A�A��B�
B  B(�B (�B(  B/�
B7�
B@  BG�
BO�
BX  B`(�BhQ�BpQ�Bx  B�
B��
B��B�(�B�  B�  B�{B�  B�  B��B�  B�{B��B�  B�{B�{B��B��B�  B��B�  B�{B�  B�  B�{B�  B�  B�  B�(�B�  B��
B�{C   C  C  C��C��C
  C  C  C
=C
=C  C��C  C
=C  C  C   C"  C#��C%��C'�C*  C,
=C.  C0  C2  C4
=C5��C8  C:  C<  C>  C@  CA��CC��CF  CH  CJ  CL
=CM��CP
=CR  CT  CV  CW��CY��C\  C^
=C`
=Cb  Cd
=Cf  Cg��Ci��Cl  Cn
=Cp  Cr
=Cs��Cv
=Cx{Cz�C|{C~  C�  C���C���C�  C�C�  C���C���C�  C�C�
=C�C���C���C�C�C�C�  C�C�C�C�
=C�C���C���C�C�C�  C�  C�C���C���C�  C���C�  C�
=C�C�  C�C�C���C���C���C�  C�  C���C�C�  C���C�  C�C�C�  C�C�
=C�  C���C���C���C�  C�  C���C�C�C���C�  C�
=C�C�  C���C�  C���C���C���C�  C�  C�C�  C���C�  C�  C�  C�C�C�C�C���C���C���C���C�  C���C�  C�  C�C�  C�  C�
=C�C�  C���C���C�  C�  C�C�C�C�C�  C�  C�C�
=C���C���C�C�\C�
=C�C�C�
=C�  C���C���C���C�C�C���C���C���D }qD �qD}qD�qD� D��D}qD  D� D  D� D�D� D  D�DD��D	  D	� D
�D
� D  D��D�D��D�qDz�D  D�D�D}qD�qD� D�D��D�D�D�qD}qD�qD� D�qDz�D��Dz�D�qDz�D�qD� D  D��D  Dz�D�qD��D  D}qD�D��D�qD� D�D� D   D � D!�D!��D"  D"��D#�D#� D$  D$��D%�D%}qD%�qD&��D'�D'��D(�D(}qD)  D)��D*  D*��D+  D+}qD,D,�D-�D-��D-�qD.� D/�D/}qD/�qD0� D1  D1}qD1��D2}qD2��D3z�D4  D4��D5�D5��D6D6}qD6��D7}qD7�qD8��D9D9}qD9�qD:}qD;  D;��D<  D<}qD<�qD=� D>  D>�D?D?��D?��D@� DADA}qDB  DB��DC  DC}qDD  DD� DE  DE� DF  DF� DG�DG� DH  DH��DIDI��DJ�DJ��DK�DK}qDL  DL��DM  DM}qDM��DNz�DN��DO}qDP  DP��DQ�DQ� DR  DR��DS  DS� DT  DT� DU�DU�DVDV��DW  DW��DX  DX� DY  DY� DZ  DZ� DZ�qD[}qD\�D\}qD]  D]��D^�D^��D^�qD_}qD_�qD`� D`�qDa}qDb  Db��Db�qDc� Dd  Ddz�Dd��De}qDe�qDf� Dg  Dg��DhDh�Di  Di}qDj  Dj� Dj�qDk}qDl  Dl��Dm  Dm}qDn  Dn}qDn��Do}qDp  Dp� Dq  Dq� Dq�qDr}qDs  Ds��Dt  Dt�DuDu� Du�qDv��Dw�Dw}qDw�qDx� Dy  Dy��Dz�Dz��D{D{� D|  D|}qD|�qD}� D~D~��D�D��D�  D�AHD���D��HD�HD�B�D���D��HD���D�@ D��HD��HD��D�B�D��HD��HD�HD�AHD�~�D���D�HD�@ D�� D��HD�  D�@ D��HD��HD�HD�@ D��HD��HD�HD�@ D�~�D�� D�  D�=qD�}qD���D���D�>�D�~�D�� D���D�AHD���D�� D�  D�AHD��HD�� D���D�@ D�� D��HD�  D�@ D��HD�� D�HD�AHD�~�D���D���D�@ D��HD��HD�HD�@ D�� D�� D���D�>�D�� D�D�  D�>�D�� D��HD�  D�>�D�}qD���D�  D�@ D�~�D���D�  D�AHD��HD���D�  D�@ D�� D�� D���D�AHD��HD���D�  D�@ D�~�D��HD�  D�@ D��HD��HD�HD�>�D�}qD��)D��qD�>�D�~�D�� D�  D�<)D�}qD���D���D�@ D�� D�� D�HD�@ D�� D��HD�  D�@ D�~�D���D�  D�>�D�� D��HD�HD�>�D�~�D���D��qD�@ D�� D�� D�  D�@ D�~�D���D�  D�>�D�� D�� D���D�@ D�~�D�� D�HD�@ D�}qD���D�  D�@ D�~�D��qD�  D�@ D��HD��HD�HD�AHD��HD��HD�HD�>�D�}qD�� D��D�@ D�� D���D�  D�B�D��HD�� D�HD�@ D�}qD���D���D�>�D�� D�� D�  D�AHD�� D�� D�  D�AHD�� D���D�  D�@ D�~�D�� D��D�AHD��HD��HD�  D�B�D��HD�� D�  D�>�D�� D��HD��D�B�D�� D���D�  D�AHD�~�D�� D���D�>�D�~�D���D�  D�>�D�~�D���D���D�>�D�� D�� D�  D�@ D�~�D���D�  D�@ D�~�D�� D��D�AHD��HD�D��D�B�D��HD���D�HD�B�D�� D�� D�HD�@ D�~�D��HD��D�>�D�~�D�� D���D�@ D D¾�D�  D�@ DÀ Dþ�D�HD�AHDāHD��HD�  D�@ D�~�DŽqD���D�B�DƁHD�� D�  D�>�D�}qD�� D�  D�@ DȀ DȾ�D��qD�=qD�}qDɾ�D�  D�>�D�~�D��HD�HD�@ D�~�D˽qD��qD�@ D́HD��HD�HD�B�D̀ D;�D���D�=qD�}qDξ�D�  D�>�D�~�D�� D�HD�B�DЁHD��HD�  D�@ DсHD�� D�  D�AHDҁHD�� D���D�>�DӀ D�� D�  D�@ DԁHD�D�HD�@ D�~�DսqD���D�@ DցHD�� D�  D�@ D�~�D׾�D��qD�@ D؁HD�� D�HD�AHD�~�DٽqD��qD�@ DځHDھ�D���D�AHDہHD�� D��D�AHD�~�DܽqD�  D�B�D݁HD��HD�HD�@ D�~�D�D��D�AHD߀ D߾�D���D�>�D�}qDྸD�HD�AHD�}qD�qD��qD�>�D� D�� D�  D�@ D� D�� D���D�>�D�~�D�� D�  D�@ D� D�� D���D�>�D� D�� D�HD�@ D� D�� D�  D�>�D�|)D辸D�HD�@ D�HD���D��D�@ D�~�D�D��D�AHD�~�D�� D�HD�AHD�~�D쾸D���D�>�D� D��HD�HD�@ D�~�D�� D�  D�@ D� D�� D�  D�@ D�� D��HD��D�AHD� D��HD�HD�@ D�HD�D�  D�AHD�HD�D�HD�B�D� D���D���D�>�D�� D���D���D�=qD�~�D�� D�HD�>�D��HG�O�>�G�?�?L��?�z�?�Q�?���?��H@�@�R@5@G�@W
=@p��@��
@��@�
=@��@���@���@�ff@�\)@��H@���@��@��HAz�A
=qA{Az�A��A{A%�A)��A-p�A333A9��A=p�AA�AHQ�AL��AQG�AW�A\(�AaG�Ag
=Aj�HAo\)AuAy��A~{A��A�33A�{A�Q�A��A�(�A�
=A���A��\A�{A��A��A��A��RA�G�A�(�A��RA�Q�A��HA�{A�  A���A���A�
=A���A��
A�{A�Q�A��A���AǮA�G�A�(�A�\)A�G�A�33AָRA���A��HA�A��A��HA�A��A�\A��A�  A��A�z�A��A�=qA�(�A�\)B ��B�B\)B��BB�HBz�B	B
�\BQ�BB�RB�
B��BffB�BG�B=qB�B��B=qB33Bz�B{B
=B (�B!�B#
=B$(�B%��B'
=B'�
B)�B*�RB+�
B,��B.ffB/�B0��B1�B3�B4��B5B733B8��B9�B:�HB<Q�B=�B>�HB@  BABB�HBD  BE��BG
=BG�
BIG�BJ�HBL(�BM�BNffBP  BP��BR{BS�BT��BU�BW33BX��BY�BZ�HB\(�B]�B^�RB`  Ba��Bb�RBc�
BeG�Bf�\Bg�Bi�Bj�\Bk�Bl��BnffBo33Bpz�Br{Bs33Bt(�BuBw33Bx(�Byp�B{
=B|Q�B}G�B
=B�{B���B�\)B�(�B���B�\)B�{B���B�G�B�{B��RB�33B��B��RB�33B��
B���B�33B��B�z�B�33B�B�Q�B��B��
B�Q�B��HB�B�ffB��HB��B�Q�B��B���B�Q�B��B��B�Q�B��B��B�z�B�
=B��
B��\B�33B�B��\B�G�B�B��\B�G�B�B�Q�B�33B��
B�Q�B��B��
B�ffB���B�B�z�B�
=B���B�Q�B�
=B���B�{B���B���B�(�B��RB��B�(�B���B�G�B�  B��RB��B��B�Q�B���B�p�B��B��\B��B�p�B��B�z�B��HB�33B�B�=qB���B��HB�33B�B�  B�(�B���B���B��B�\)B�B�{B�=qB�z�B�
=B�\)B���B�B�=qB��\B���B�
=B�p�B��B�{B�=qB£�B��B�G�B�p�B��
B�=qB�ffBĸRB��B�\)Bř�B��B�ffBƏ\B��HB�\)B�B�  B�=qBȣ�B�
=BɅB��
B�  B�ffB��HB�G�B˅B�B�=qḄ�B���B�33BͅB�{B�ffBθRB��HB�G�B�B�{B�Q�BЏ\B��BхB�B��B�ffB��HB�33B�p�B�B�=qBԸRB�
=B�G�Bՙ�B�{B֏\B���B�G�BׅB��B�ffBظRB�
=B�G�B��
B�=qB�z�B���B�33BۮB�  B�=qBܣ�B��B݅BݮB�  B�z�B��HB�
=B�\)B��B�Q�B��\B�RB��BᙚB�  B�Q�B�\B���B�G�B�B�{B�Q�B�\B���B�p�B�B�  B�=qB�RB��B�p�B癚B�{B�z�B�RB��HB�p�B�B�{B�Q�B�\B�
=B�p�B�B��B�(�B�\B�
=B�G�B�B��B�{B�\B��HB��B�\)B��
B�=qB�z�B�RB�
=B�B��
B�  B�ffB���B�
=B�G�B�B�  B�Q�B�\B��RB��B�p�B��
B�(�B�Q�B��\B���B�\)B��B��B�  B�ffB��HB�33B�p�B��B�(�B���B��HB��B���B�{B�Q�B���B�
=B��B�B�  B�z�B���B�33B��B��
C �C \)C �C ��C �
C{C33CQ�C�CC�C  C33Cp�C��CC�HC
=CG�C�C��C�RC��C33CQ�Cp�C��C�C
=C(�CQ�C��C�RC�
C  C=qCp�C�C�C�C{C(�CQ�C�\C�RC��C�C(�CQ�Cz�C�\C�C�HC	{C	(�C	G�C	�C	�C	�RC	�C
�C
=qC
Q�C
z�C
�RC
�
C
�C�CG�CQ�Cz�C�RC�HC��C{CQ�Cz�C�C�RC��C  C(�C\)C�C��C��C
=C�CG�Cz�C�C�
C�C�C\)Cz�C�\C��C  C(�C=qCffC��C��C�C
=C33Cp�C��C�C�
C{C33CQ�C�C�RC��C��C33CQ�CffC��C�
C��C
=CG�Cz�C��C�RC�C(�C=qC\)C�\C�RC�C{C(�CQ�Cz�C�C�HC{C33CG�Cz�C�RC�C  C(�C\)C�\C�RC��C
=CG�Cz�C��CC�C(�CffC�\C��C�HC�CQ�Cp�C�\C�
C
=C33CQ�C��C��C��C{CQ�C��C��C��C�C\)C��C��C��C(�Cp�C�C�RC�C 33C ffC �\C �RC ��C!33C!\)C!z�C!�C!��C"33C"\)C"z�C"�RC#  C#=qC#ffC#�\C#��C${C$G�C$p�C$��C$�
C%�C%ffC%��C%�RC%��C&=qC&�C&C&�C'�C'Q�C'�C'C(
=C(=qC(p�C(��C(�
C)�C)Q�C)z�C)�RC)��C*G�C*p�C*��C*�
C+{C+ffC+��C+C,
=C,Q�C,�\C,�RC,�C-(�C-z�C-�RC-�C.�C.G�C.�C.��C/
=C/G�C/z�C/��C/�HC0(�C0ffC0��C0��C0��C133C1z�C1�RC1�C2�C2G�C2�C2�
C3{C3G�C3�C3�RC3�
C4{C4\)C4��C4��C4��C5�C5ffC5�C5�HC6
=C633C6p�C6��C6�HC7�C7\)C7��C7��C7��C8(�C8z�C8�RC8�HC9
=C9=qC9z�C9C9�C:{C:=qC:p�C:C;  C;(�C;Q�C;�C;�C;�
C<{C<Q�C<�\C<C<�HC=
=C==qC=z�C=�RC=�HC>
=C>=qC>p�C>��C>�HC?{C?G�C?z�C?�C?�HC@
=C@=qC@\)C@�C@CA  CA33CAffCA�\CACA�CB
=CB=qCBffCB��CB�
CC
=CC=qCCp�CC��CC��CD
=CD=qCDffCD��CD�
CE  CE33CEp�CE��CE��CE��CF33CFffCF��CF��CF��CG33CGffCG��CGCH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                 @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ƨA�AռjA�ĜA�ĜA�ȴA���A���A���A���A���A���A���A��
A���A���A�ĜA���Aՙ�A�~�A�ffA�ZA�VA�S�A�M�A�K�A�C�A�;dA�33A�"�A�{A�oA�bA�bA�VA�VA�A�A���A���A���A�  A�A���A��A��Aԧ�A�M�A�  A��A��A�1A�{A��`A�dZA�A�AƮA�5?AŴ9A�=qA�VA��TAģ�A��A�A�A�A�+A�x�A��#A��A� �A��hA�`BA��HA���A��`A��A�(�A�ƨA�-A�\)A��A��mA��9A��FA���A��A��^A��A�dZA�5?A�-A�\)A���A�M�A�VA�;dA��RA��`A��DA�?}A�K�A�%A�ƨA�I�A�~�A�jA|�!AvI�At�Ar(�Aot�AnI�AlffAj9XAgt�Aep�Acp�A_��A[AY�AW"�AT��AR^5AN�`AJ{AG�AF�uAF{AC�A@�9A?&�A=�;A<ZA;G�A7�mA6��A4��A3��A2��A1/A/&�A.�yA-�A+��A*�yA*{A(��A'oA&�A&bA%�^A$ĜA �/A�A�AM�AM�A33A��A��A��A|�AhsA�A��Av�A�hAA{A~�AXA
Q�A	��A	VAJA�A^5A�;Ap�A�A��AO�A+A1AXA �@��@��9@���@�?}@�  @���@���@���@��`@�w@��@�/@�p�@�&�@��@�9@�(�@�+@��^@��
@��@�+@�R@�^5@�@��@��@�/@�X@�V@�9@� �@���@�\)@�O�@�A�@�\)@އ+@�~�@���@��
@߶F@ߍP@ߍP@�t�@�\)@�K�@�C�@�@�v�@���@�@�M�@ڧ�@�C�@��;@�V@ܛ�@ّh@�1@���@�V@�{@�-@���@պ^@�hs@��@�j@Ӯ@��
@��;@�K�@�ȴ@�M�@���@�{@��y@���@�p�@���@щ7@���@��@�33@Ώ\@͑h@��`@̋D@�Z@�Z@�Q�@�9X@�(�@�K�@�@�{@ɩ�@ɉ7@�x�@�X@ȋD@�ƨ@���@�b@�ȴ@\@�V@�@���@��@°!@+@�ff@�J@���@�|�@�J@�x�@��`@��j@��u@�I�@�+@�x�@��@��`@��`@��j@��@�bN@�b@�|�@���@�E�@��#@��^@���@��7@��@�X@�V@��@���@���@��@�j@��@�b@��@�dZ@��@��\@�ff@�M�@�5?@��@��-@�?}@���@���@��`@��/@��/@�9X@���@��@�"�@���@���@���@��R@���@��+@�^5@�M�@�J@�@�p�@�7L@���@�9X@�1'@���@��@�@���@��\@���@��7@�%@���@���@��/@��j@��@�1@��@�S�@�K�@�C�@�"�@�^5@�{@���@�O�@�&�@��@�A�@��@�  @�ƨ@���@��P@��@�K�@���@�v�@��@���@�/@��j@�r�@��@�ƨ@��@�o@��H@��!@�n�@��@�G�@���@���@��9@���@��@�Q�@��P@��@���@�ȴ@�n�@��T@�7L@��@�z�@��m@���@�C�@�n�@���@�?}@��`@��@� �@�ƨ@���@�l�@�K�@�"�@��@�ȴ@�ff@�@�`B@��@��@�r�@�Q�@�b@�l�@��@�~�@�n�@�M�@��@�@��-@���@��h@��h@�p�@�%@���@��u@�Q�@�9X@��m@�C�@��@���@�n�@�V@�=q@�J@��7@���@�Z@���@�K�@��@���@�v�@�ff@�V@�=q@��@�J@���@���@�@���@��7@�p�@�X@�/@���@�I�@�  @��@��@�l�@�C�@���@��H@���@���@�v�@�E�@��@���@�@�`B@�/@���@���@���@��@�Q�@�1@|�@\)@
=@~�@~�R@~�+@~E�@}��@|��@|�@|�/@|��@|I�@{dZ@z�@z�H@z�H@z��@z��@z��@y��@y�@y�^@yx�@x��@xr�@xbN@xQ�@w�;@w|�@wK�@w
=@v�y@vv�@u�@u�@u�T@u�h@up�@u/@t�j@t9X@sƨ@sC�@so@s@r�H@r~�@q��@q7L@p��@p��@pr�@pA�@p �@o��@n�+@nE�@n@m�h@m�@kƨ@kt�@kdZ@k"�@j~�@iG�@h��@h�9@h�@hb@g+@fff@e?}@d9X@c��@c33@b�@b�!@b-@a��@a��@ax�@ax�@aX@`��@`��@`��@`Ĝ@`�u@`r�@`A�@` �@_�@_�P@^�+@]`B@\Z@[C�@Z��@Z�\@Z=q@Y�7@YX@XĜ@XQ�@X �@W�;@Wl�@W
=@V�@V�@V�@V��@VV@U�T@Up�@TI�@SdZ@R^5@R�@RJ@RJ@Q��@Q��@Q�^@QG�@P��@Pb@O�;@O�w@N�@M��@M��@M�h@MO�@MV@L�@L�D@Lj@LZ@LI�@L9X@L�@K��@K�F@K��@Kt�@KdZ@KC�@K"�@K@J�!@J^5@J-@I�#@H�9@Hb@G�P@G;d@G�@F�@F��@F��@FE�@F@E�T@E�@EO�@EV@D�@D�D@DI�@C�F@CS�@Co@B��@BM�@B�@A�@A�#@A��@AX@@�`@@Ĝ@@Ĝ@@Ĝ@@Ĝ@@��@@Q�@@A�@?�@?+@>ff@=�-@<�@<9X@;��@:�@9��@9%@8�9@7��@7
=@6v�@5��@5��@5?}@5V@4�/@4��@4��@4��@4�j@4��@4z�@49X@4(�@41@3�m@3�F@3S�@3"�@3"�@3@2�!@1��@1X@1G�@17L@0��@0A�@/�@/��@/|�@/\)@/K�@/+@/+@/+@/�@/
=@/
=@/
=@.��@.�y@.�+@-��@-�@-p�@-`B@-/@,�D@+S�@*��@*=q@)��@)�7@)x�@)G�@)7L@)&�@)%@(�`@(��@(��@(�9@(r�@(A�@'�P@'+@&�y@&v�@&E�@%�@%��@%�h@$�/@$�D@$Z@$(�@#�F@#33@"��@"~�@"-@!�#@!x�@!�@ �`@ �`@ �`@ ��@ r�@ 1'@   @��@�@�P@|�@\)@;d@�y@v�@5?@�@@�-@�-@�-@��@`B@/@V@��@�@�@�@�@�/@�@z�@Z@1@�F@��@dZ@o@�@��@M�@�#@G�@�@�`@��@Q�@A�@b@�@��@�P@|�@\)@�@��@5?@�h@?}@?}@/@�@��@�j@�D@j@(�@�@��@�m@��@C�@"�@@�@��@n�@M�@-@�@�@J@��@��@�@�^@��@�7@x�@x�@x�@hs@X@G�@7L@&�@�@%@��@Ĝ@��@�@Q�@�;@�w@�@�P@�P@�P@�P@�P@\)@
=@�y@ȴ@�R@��@v�@ff@$�@��@��@�@�@`B@O�@��@��@��@j@I�@(�@�@1@1@��@�
@�F@��@�@S�@@
��@
n�@
^5@
M�@
^5@
^5@
=q@
�@	�7@	7L@	7L@	�@��@Ĝ@�9@�9@��@�@r�@bN@Q�@Q�@ �@b@�@�w@l�@��@��@ff@ff@5?@�T@@��@�hA�ƨA�ƨA�ƨA�ƨA�ƨA���A�ȴA�ĜAվwAվwAռjAռjA�ȴA�ĜAվwA�ĜA�ȴA�ĜA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A��
A���A���A��
A���A���A���A���A���A��#A��
A��
A��#A���A��
A���A�ƨA���A��
A���A���A��#A��
A���A��A���A���AնFA�AռjA���A�ƨA�ĜA�A�ȴA�ƨAվwA�ĜA�ĜAվwAվwA���A���Aթ�A՟�Aա�AՍPAՉ7AՏ\AՍPAՇ+AՇ+A�|�A�t�A�t�A�v�A�n�A�p�A�p�A�ZA�VA�\)A�ZA�VA�\)A�\)A�VA�XA�\)A�VA�S�A�XA�VA�Q�A�S�A�VA�S�A�S�A�VA�M�A�O�A�Q�A�M�A�K�A�M�A�O�A�I�A�I�A�O�A�K�A�I�A�M�A�K�A�G�A�G�A�G�A�C�A�?}A�C�A�?}A�=qA�?}A�?}A�7LA�5?A�7LA�33A�1'A�33A�5?A�5?A�1'A�33A�5?A�+A��A��A��A�bA�{A��A�oA�{A��A��A�oA�{A�{A�bA�bA�{A�{A�VA�bA�oA�bA�
=A�JA�oA�bA�VA�oA�oA�VA�oA�VA�
=A�bA�oA�VA�VA�oA�JA�JA�bA�VA�A�1A�%A�A�A�A�A�  A�%A�A���A���A�A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A�A�A���A���A�  A�A���A�  A�A�A�  A�  A�A�A���A���A�A���A���A���A���A��A��A��A��A��A��yA��mA��TA��A���A���A�ĜAԾwAԺ^A԰!Aԗ�Aԕ�AԑhA�p�A�`BA�M�A�G�A�7LA�(�A�1'A�/A�VA�  A��TA���Aӛ�AӁA�XA�(�A���A�l�A�oAѴ9A�K�A�A���A�|�A�7LA���A�7LA���A�I�A�bA���A͍PA��yA�ĜA̺^A̮A�~�A�I�A�-A�VA��A��A�ĜA˲-Aˡ�A˓uA˃A�jA�bNA�^5A�XA�E�A��A��/AʓuA�bNA�33A�{A�A��A���Aɛ�A�|�A�VA�I�A�E�A�C�A�=qA�;dA�=qA�;dA�5?A�5?A�7LA�-A�JAȶFA�r�A�S�A���A���Aǡ�A�v�A�&�A�
=A�%A��`A���AƶFAƟ�Aƛ�AƗ�AƉ7A�v�A�hsA�`BA�XA�M�A�C�A�=qA�=qA�;dA�/A�&�A�"�A��A�{A�A���A��A��TA��
A�ĜAŮAŗ�AŅA�z�A�bNA�^5A�`BA�\)A�G�A�9XA�1'A�1'A�1'A�(�A� �A�$�A�$�A��A��A��A� �A��A��A�
=A�  A���A��yA��`A��mA��`A��HA��HA��`A��`A��TA��;A��HA��TA��`A��`A��#A��
A���A�ĜAİ!AđhAāA�n�A�ZA�Q�A�C�A�-A��A�1A��A��mA��mA��HA���A���AøRAç�AÑhA�|�A�n�A�`BA�I�A�+A�oA��A���Aº^A�AA�dZA�?}A�VA��/A��A�jA�9XA�
=A��/A���A�hsA��A��;A��!A�|�A�bNA�A�A�(�A�"�A�$�A� �A�JA��#A��FA�v�A��A�n�A��A��yA��/A�ĜA���A�M�A��A��FA�n�A�=qA�VA���A���A�VA�1A��A���A��DA�XA�"�A��A�A���A�n�A�I�A�+A�bA��A���A��wA��A���A�~�A�jA�XA�G�A�5?A� �A�bA�A��A��HA��A��jA���A���A��uA��uA��A�jA�C�A�VA���A��`A���A�ƨA���A��9A��A���A���A���A��\A��PA��DA��A�|�A�v�A�t�A�p�A�l�A�jA�l�A�ffA�`BA�^5A�^5A�^5A�\)A�XA�O�A�I�A�?}A�-A��A�1A���A��`A��/A���A���A�ĜA��RA��-A���A��PA�v�A�9XA�  A���A��!A���A��hA�|�A�bNA�G�A�$�A��A�A��A��HA�bA�A���A���A��7A�jA�G�A�1'A��A�JA���A��TA��A�ȴA��^A��FA��-A���A���A���A��7A��A�r�A�hsA�dZA�ffA�K�A�5?A�-A�"�A��A�oA�VA�%A��A��yA��`A��;A��#A��#A��#A��
A�ȴA��wA��FA���A���A��DA�t�A�K�A�A�A�=qA�;dA�7LA�/A� �A��A�1A�  A���A��A���A��RA�=qA�Q�A��mA��#A���A��-A���A��uA�~�A�hsA�A�A�  A��A�  A��A��A�^5A�&�A���A�t�A�?}A��A���A��A��A��mA��HA���A�ƨA��RA���A�|�A�O�A�bA���A��uA�v�A�^5A�I�A�1'A��A��A�ƨA��RA��-A��!A��A���A���A���A���A��7A�dZA�O�A�M�A�M�A�C�A�
=A�=qA�bA��A���A�XA�VA��-A�p�A�=qA��A�%A���A���A��yA��HA���A��jA���A��uA�x�A�n�A�hsA�ffA�`BA�^5A�\)A�S�A��A�ZA�1A��A�ȴA��!A��uA�t�A�O�A��A���A��/A��A���A��A��!A�ZA���A���A�A�A�bA��A�ƨA���A�Q�A�bA���A��\A�bNA�(�A���A���A���A�t�A�I�A�&�A���A��FA�XA�$�A��A��PA�  A�S�A�ƨA�ffA�(�A���A��
A���A�t�A�\)A�;dA�&�A��A��jA��FA���A���A���A���A�~�A�K�A�bA���A��!A�x�A�/A�ĜA�r�A��A���A��hA�jA�5?A��A��;A��;A��HA��/A��A���A���A�S�A��A���A�z�A�"�A�  A��#A��A��DA�|�A�l�A�ffA�ZA�33A�JA��A��jA�~�A�XA��A��A��RA���A��A�t�A�^5A�I�A�9XA�+A�bA���A���A��
A��-A��A�E�A��A���A�?}A��9A�1'A��A�/A��A��^A��DA�?}A�
=A��#A��9A���A��DA�n�A�XA��A�
=A��A���A���A���A���A��A�t�A�ZA�/A��/A�|�A�33A� �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                 A�ƨA�AռjA�ĜA�ĜA�ȴA���A���A���A���A���A���A���A��
A���A���A�ĜA���Aՙ�A�~�A�ffA�ZA�VA�S�A�M�A�K�A�C�A�;dA�33A�"�A�{A�oA�bA�bA�VA�VA�A�A���A���A���A�  A�A���A��A��Aԧ�A�M�A�  A��A��A�1A�{A��`A�dZA�A�AƮA�5?AŴ9A�=qA�VA��TAģ�A��A�A�A�A�+A�x�A��#A��A� �A��hA�`BA��HA���A��`A��A�(�A�ƨA�-A�\)A��A��mA��9A��FA���A��A��^A��A�dZA�5?A�-A�\)A���A�M�A�VA�;dA��RA��`A��DA�?}A�K�A�%A�ƨA�I�A�~�A�jA|�!AvI�At�Ar(�Aot�AnI�AlffAj9XAgt�Aep�Acp�A_��A[AY�AW"�AT��AR^5AN�`AJ{AG�AF�uAF{AC�A@�9A?&�A=�;A<ZA;G�A7�mA6��A4��A3��A2��A1/A/&�A.�yA-�A+��A*�yA*{A(��A'oA&�A&bA%�^A$ĜA �/A�A�AM�AM�A33A��A��A��A|�AhsA�A��Av�A�hAA{A~�AXA
Q�A	��A	VAJA�A^5A�;Ap�A�A��AO�A+A1AXA �@��@��9@���@�?}@�  @���@���@���@��`@�w@��@�/@�p�@�&�@��@�9@�(�@�+@��^@��
@��@�+@�R@�^5@�@��@��@�/@�X@�V@�9@� �@���@�\)@�O�@�A�@�\)@އ+@�~�@���@��
@߶F@ߍP@ߍP@�t�@�\)@�K�@�C�@�@�v�@���@�@�M�@ڧ�@�C�@��;@�V@ܛ�@ّh@�1@���@�V@�{@�-@���@պ^@�hs@��@�j@Ӯ@��
@��;@�K�@�ȴ@�M�@���@�{@��y@���@�p�@���@щ7@���@��@�33@Ώ\@͑h@��`@̋D@�Z@�Z@�Q�@�9X@�(�@�K�@�@�{@ɩ�@ɉ7@�x�@�X@ȋD@�ƨ@���@�b@�ȴ@\@�V@�@���@��@°!@+@�ff@�J@���@�|�@�J@�x�@��`@��j@��u@�I�@�+@�x�@��@��`@��`@��j@��@�bN@�b@�|�@���@�E�@��#@��^@���@��7@��@�X@�V@��@���@���@��@�j@��@�b@��@�dZ@��@��\@�ff@�M�@�5?@��@��-@�?}@���@���@��`@��/@��/@�9X@���@��@�"�@���@���@���@��R@���@��+@�^5@�M�@�J@�@�p�@�7L@���@�9X@�1'@���@��@�@���@��\@���@��7@�%@���@���@��/@��j@��@�1@��@�S�@�K�@�C�@�"�@�^5@�{@���@�O�@�&�@��@�A�@��@�  @�ƨ@���@��P@��@�K�@���@�v�@��@���@�/@��j@�r�@��@�ƨ@��@�o@��H@��!@�n�@��@�G�@���@���@��9@���@��@�Q�@��P@��@���@�ȴ@�n�@��T@�7L@��@�z�@��m@���@�C�@�n�@���@�?}@��`@��@� �@�ƨ@���@�l�@�K�@�"�@��@�ȴ@�ff@�@�`B@��@��@�r�@�Q�@�b@�l�@��@�~�@�n�@�M�@��@�@��-@���@��h@��h@�p�@�%@���@��u@�Q�@�9X@��m@�C�@��@���@�n�@�V@�=q@�J@��7@���@�Z@���@�K�@��@���@�v�@�ff@�V@�=q@��@�J@���@���@�@���@��7@�p�@�X@�/@���@�I�@�  @��@��@�l�@�C�@���@��H@���@���@�v�@�E�@��@���@�@�`B@�/@���@���@���@��@�Q�@�1@|�@\)@
=@~�@~�R@~�+@~E�@}��@|��@|�@|�/@|��@|I�@{dZ@z�@z�H@z�H@z��@z��@z��@y��@y�@y�^@yx�@x��@xr�@xbN@xQ�@w�;@w|�@wK�@w
=@v�y@vv�@u�@u�@u�T@u�h@up�@u/@t�j@t9X@sƨ@sC�@so@s@r�H@r~�@q��@q7L@p��@p��@pr�@pA�@p �@o��@n�+@nE�@n@m�h@m�@kƨ@kt�@kdZ@k"�@j~�@iG�@h��@h�9@h�@hb@g+@fff@e?}@d9X@c��@c33@b�@b�!@b-@a��@a��@ax�@ax�@aX@`��@`��@`��@`Ĝ@`�u@`r�@`A�@` �@_�@_�P@^�+@]`B@\Z@[C�@Z��@Z�\@Z=q@Y�7@YX@XĜ@XQ�@X �@W�;@Wl�@W
=@V�@V�@V�@V��@VV@U�T@Up�@TI�@SdZ@R^5@R�@RJ@RJ@Q��@Q��@Q�^@QG�@P��@Pb@O�;@O�w@N�@M��@M��@M�h@MO�@MV@L�@L�D@Lj@LZ@LI�@L9X@L�@K��@K�F@K��@Kt�@KdZ@KC�@K"�@K@J�!@J^5@J-@I�#@H�9@Hb@G�P@G;d@G�@F�@F��@F��@FE�@F@E�T@E�@EO�@EV@D�@D�D@DI�@C�F@CS�@Co@B��@BM�@B�@A�@A�#@A��@AX@@�`@@Ĝ@@Ĝ@@Ĝ@@Ĝ@@��@@Q�@@A�@?�@?+@>ff@=�-@<�@<9X@;��@:�@9��@9%@8�9@7��@7
=@6v�@5��@5��@5?}@5V@4�/@4��@4��@4��@4�j@4��@4z�@49X@4(�@41@3�m@3�F@3S�@3"�@3"�@3@2�!@1��@1X@1G�@17L@0��@0A�@/�@/��@/|�@/\)@/K�@/+@/+@/+@/�@/
=@/
=@/
=@.��@.�y@.�+@-��@-�@-p�@-`B@-/@,�D@+S�@*��@*=q@)��@)�7@)x�@)G�@)7L@)&�@)%@(�`@(��@(��@(�9@(r�@(A�@'�P@'+@&�y@&v�@&E�@%�@%��@%�h@$�/@$�D@$Z@$(�@#�F@#33@"��@"~�@"-@!�#@!x�@!�@ �`@ �`@ �`@ ��@ r�@ 1'@   @��@�@�P@|�@\)@;d@�y@v�@5?@�@@�-@�-@�-@��@`B@/@V@��@�@�@�@�@�/@�@z�@Z@1@�F@��@dZ@o@�@��@M�@�#@G�@�@�`@��@Q�@A�@b@�@��@�P@|�@\)@�@��@5?@�h@?}@?}@/@�@��@�j@�D@j@(�@�@��@�m@��@C�@"�@@�@��@n�@M�@-@�@�@J@��@��@�@�^@��@�7@x�@x�@x�@hs@X@G�@7L@&�@�@%@��@Ĝ@��@�@Q�@�;@�w@�@�P@�P@�P@�P@�P@\)@
=@�y@ȴ@�R@��@v�@ff@$�@��@��@�@�@`B@O�@��@��@��@j@I�@(�@�@1@1@��@�
@�F@��@�@S�@@
��@
n�@
^5@
M�@
^5@
^5@
=q@
�@	�7@	7L@	7L@	�@��@Ĝ@�9@�9@��@�@r�@bN@Q�@Q�@ �@b@�@�w@l�@��@��@ff@ff@5?@�T@@��G�O�A�ƨA�ƨA�ƨA�ƨA�ƨA���A�ȴA�ĜAվwAվwAռjAռjA�ȴA�ĜAվwA�ĜA�ȴA�ĜA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A��
A���A���A��
A���A���A���A���A���A��#A��
A��
A��#A���A��
A���A�ƨA���A��
A���A���A��#A��
A���A��A���A���AնFA�AռjA���A�ƨA�ĜA�A�ȴA�ƨAվwA�ĜA�ĜAվwAվwA���A���Aթ�A՟�Aա�AՍPAՉ7AՏ\AՍPAՇ+AՇ+A�|�A�t�A�t�A�v�A�n�A�p�A�p�A�ZA�VA�\)A�ZA�VA�\)A�\)A�VA�XA�\)A�VA�S�A�XA�VA�Q�A�S�A�VA�S�A�S�A�VA�M�A�O�A�Q�A�M�A�K�A�M�A�O�A�I�A�I�A�O�A�K�A�I�A�M�A�K�A�G�A�G�A�G�A�C�A�?}A�C�A�?}A�=qA�?}A�?}A�7LA�5?A�7LA�33A�1'A�33A�5?A�5?A�1'A�33A�5?A�+A��A��A��A�bA�{A��A�oA�{A��A��A�oA�{A�{A�bA�bA�{A�{A�VA�bA�oA�bA�
=A�JA�oA�bA�VA�oA�oA�VA�oA�VA�
=A�bA�oA�VA�VA�oA�JA�JA�bA�VA�A�1A�%A�A�A�A�A�  A�%A�A���A���A�A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A�A�A���A���A�  A�A���A�  A�A�A�  A�  A�A�A���A���A�A���A���A���A���A��A��A��A��A��A��yA��mA��TA��A���A���A�ĜAԾwAԺ^A԰!Aԗ�Aԕ�AԑhA�p�A�`BA�M�A�G�A�7LA�(�A�1'A�/A�VA�  A��TA���Aӛ�AӁA�XA�(�A���A�l�A�oAѴ9A�K�A�A���A�|�A�7LA���A�7LA���A�I�A�bA���A͍PA��yA�ĜA̺^A̮A�~�A�I�A�-A�VA��A��A�ĜA˲-Aˡ�A˓uA˃A�jA�bNA�^5A�XA�E�A��A��/AʓuA�bNA�33A�{A�A��A���Aɛ�A�|�A�VA�I�A�E�A�C�A�=qA�;dA�=qA�;dA�5?A�5?A�7LA�-A�JAȶFA�r�A�S�A���A���Aǡ�A�v�A�&�A�
=A�%A��`A���AƶFAƟ�Aƛ�AƗ�AƉ7A�v�A�hsA�`BA�XA�M�A�C�A�=qA�=qA�;dA�/A�&�A�"�A��A�{A�A���A��A��TA��
A�ĜAŮAŗ�AŅA�z�A�bNA�^5A�`BA�\)A�G�A�9XA�1'A�1'A�1'A�(�A� �A�$�A�$�A��A��A��A� �A��A��A�
=A�  A���A��yA��`A��mA��`A��HA��HA��`A��`A��TA��;A��HA��TA��`A��`A��#A��
A���A�ĜAİ!AđhAāA�n�A�ZA�Q�A�C�A�-A��A�1A��A��mA��mA��HA���A���AøRAç�AÑhA�|�A�n�A�`BA�I�A�+A�oA��A���Aº^A�AA�dZA�?}A�VA��/A��A�jA�9XA�
=A��/A���A�hsA��A��;A��!A�|�A�bNA�A�A�(�A�"�A�$�A� �A�JA��#A��FA�v�A��A�n�A��A��yA��/A�ĜA���A�M�A��A��FA�n�A�=qA�VA���A���A�VA�1A��A���A��DA�XA�"�A��A�A���A�n�A�I�A�+A�bA��A���A��wA��A���A�~�A�jA�XA�G�A�5?A� �A�bA�A��A��HA��A��jA���A���A��uA��uA��A�jA�C�A�VA���A��`A���A�ƨA���A��9A��A���A���A���A��\A��PA��DA��A�|�A�v�A�t�A�p�A�l�A�jA�l�A�ffA�`BA�^5A�^5A�^5A�\)A�XA�O�A�I�A�?}A�-A��A�1A���A��`A��/A���A���A�ĜA��RA��-A���A��PA�v�A�9XA�  A���A��!A���A��hA�|�A�bNA�G�A�$�A��A�A��A��HA�bA�A���A���A��7A�jA�G�A�1'A��A�JA���A��TA��A�ȴA��^A��FA��-A���A���A���A��7A��A�r�A�hsA�dZA�ffA�K�A�5?A�-A�"�A��A�oA�VA�%A��A��yA��`A��;A��#A��#A��#A��
A�ȴA��wA��FA���A���A��DA�t�A�K�A�A�A�=qA�;dA�7LA�/A� �A��A�1A�  A���A��A���A��RA�=qA�Q�A��mA��#A���A��-A���A��uA�~�A�hsA�A�A�  A��A�  A��A��A�^5A�&�A���A�t�A�?}A��A���A��A��A��mA��HA���A�ƨA��RA���A�|�A�O�A�bA���A��uA�v�A�^5A�I�A�1'A��A��A�ƨA��RA��-A��!A��A���A���A���A���A��7A�dZA�O�A�M�A�M�A�C�A�
=A�=qA�bA��A���A�XA�VA��-A�p�A�=qA��A�%A���A���A��yA��HA���A��jA���A��uA�x�A�n�A�hsA�ffA�`BA�^5A�\)A�S�A��A�ZA�1A��A�ȴA��!A��uA�t�A�O�A��A���A��/A��A���A��A��!A�ZA���A���A�A�A�bA��A�ƨA���A�Q�A�bA���A��\A�bNA�(�A���A���A���A�t�A�I�A�&�A���A��FA�XA�$�A��A��PA�  A�S�A�ƨA�ffA�(�A���A��
A���A�t�A�\)A�;dA�&�A��A��jA��FA���A���A���A���A�~�A�K�A�bA���A��!A�x�A�/A�ĜA�r�A��A���A��hA�jA�5?A��A��;A��;A��HA��/A��A���A���A�S�A��A���A�z�A�"�A�  A��#A��A��DA�|�A�l�A�ffA�ZA�33A�JA��A��jA�~�A�XA��A��A��RA���A��A�t�A�^5A�I�A�9XA�+A�bA���A���A��
A��-A��A�E�A��A���A�?}A��9A�1'A��A�/A��A��^A��DA�?}A�
=A��#A��9A���A��DA�n�A�XA��A�
=A��A���A���A���A���A��A�t�A�ZA�/A��/A�|�A�33A� �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                 ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BxBxBBJBBDBB�BBxB�BDBDBDBDB�BDBDB�B�B�BbB�BbB�B�B�BVB�B�B�B\B.B4B�BoBB:BB�BB�BBFB�B.B
rB�B
��B
�B
�cB(B �B�BR�B��B�aBӏB�B�5B��B�B��B�VB��B�B��B��B�AB�AB�B�;B�B�B�DB�B��B��B��B�5B��B�
B�)B�fB�B�yBԕB�zB�^B�zB��B��B��BpoBW�B6�B.}B!�B	�B
�8B
�B
��B
�B
�B
�@B
��B
r�B
a�B
>�B
�B
	B	�cB	�B	�B	�NB	�2B	��B	��B	��B	��B	��B	zB	jKB	e,B	P�B	S�B	8B	+kB	#nB	~B	qB	�B	B	
=B	YB		lB	�B	�B	eB	�B	�B	�B	~B		7B	JB	�B	YB	�B	B	4B	.B	�B	�B	kB	�B	�B	(�B	(�B	@B	_B�fB�NB�EBخB�EB�QB�dB��B�B� B�,B�cB��B�(B	 iB	oB	B��B��B�PB	 iB	+B	VB	 B	�B	MB	FB	�B	�B	�B	_B	 \B	$B	#�B	&B	*0B	.B	/OB	2-B	1�B	8�B	E9B	HB	H�B	J�B	N�B	P�B	PB	Q�B	O�B	OvB	PHB	R B	S[B	TaB	U2B	XEB	[#B	[#B	ZB	W�B	W
B	TaB	OvB	O�B	QNB	RTB	bNB	gmB	iyB	k�B	s�B	|PB	}�B	}�B	|�B	{JB	w�B	w2B	tTB	w�B	xlB	~�B	��B	�JB	�uB	��B	�uB	��B	��B	��B	�B	�YB	��B	��B	��B	��B	��B	��B	�~B	�DB	�=B	�PB	�(B	�hB	�FB	�'B	��B	��B	�FB	��B	��B	�9B	��B	�^B	�XB	��B	�B	��B	�B	҉B	רB	�B	ٴB	�)B	��B	��B	��B	�jB	��B	��B	�]B	�&B	ϫB	�aB	�B	�KB	�jB	�ZB	�`B	�B	�B	�sB	�WB	�"B	�B	�B	�B	�QB	�B	��B	��B	�B	�]B	�/B	��B	�B	�iB	�;B	�B	�oB	�AB	�B	�B	�vB	�AB	��B	�GB	�MB	��B	��B	�ZB	�B	��B	�B	�PB	��B	�PB	�JB	�PB	��B	��B
 iB
 �B
B
B
�B
�B
�B
�B
MB
B
fB
�B
�B
	lB
	�B

=B

	B

=B

rB

rB

�B

�B
xB
�B
�B
B
"B
�B
�B
"B
�B
�B
�B
4B
�B
uB
uB
uB
@B
@B
@B
�B
�B
�B
�B
�B
{B
{B
SB
SB
�B
YB
�B
B
B
7B
kB
	B
�B
�B
kB
=B
CB
IB
~B
�B
�B
VB
�B
 �B
!-B
!bB
"4B
"4B
"�B
#B
#:B
#�B
$@B
$tB
$�B
$�B
$�B
$�B
&LB
'RB
'�B
'�B
'�B
(�B
)�B
)�B
*0B
*�B
+6B
,=B
.IB
.�B
/�B
/�B
0�B
1'B
1�B
1�B
2-B
2aB
2�B
33B
33B
3�B
4�B
5�B
6B
6B
5�B
5�B
5�B
6�B
7�B
7�B
7�B
7�B
8�B
9$B
9XB
9XB
9�B
9XB
9�B
;�B
;�B
<B
<B
;dB
;�B
<6B
;�B
;�B
;�B
;�B
;�B
<6B
=<B
>BB
>wB
?B
?}B
?�B
@�B
@�B
@�B
A B
A�B
A�B
A�B
A�B
A�B
AUB
B'B
A�B
A�B
B'B
B'B
B�B
C�B
C�B
C�B
D�B
D�B
EB
E�B
E�B
FB
F?B
FtB
F�B
F�B
GB
GzB
H�B
H�B
H�B
IRB
I�B
I�B
I�B
J�B
J�B
J�B
K)B
J�B
J�B
J�B
K^B
K�B
K�B
K�B
K�B
L0B
LdB
M�B
M�B
M�B
MjB
M�B
M6B
M6B
N<B
NB
N<B
N<B
OB
OBB
OBB
OvB
O�B
P�B
Q�B
Q�B
Q�B
R B
RTB
R B
R B
R�B
R�B
R�B
S[B
S�B
T,B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
VB
VmB
VmB
V�B
VmB
W
B
XB
XB
XEB
XyB
YB
ZQB
ZQB
ZQB
ZQB
[#B
[�B
\)B
[�B
[�B
\]B
\�B
]/B
^jB
^�B
_pB
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
aB
a|B
a|B
a|B
a|B
a�B
a�B
a�B
a�B
a�B
a|B
bNB
b�B
c B
dZB
d�B
d�B
d�B
e�B
e�B
f�B
f�B
gB
gB
g�B
gmB
g�B
g�B
gmB
g�B
g�B
gmB
g�B
hsB
iDB
jB
i�B
i�B
i�B
i�B
i�B
jB
jKB
j�B
kQB
j�B
j�B
l"B
l�B
l�B
l�B
l�B
l�B
m)B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
n/B
ncB
ncB
n�B
n�B
n�B
o B
o B
n�B
o5B
p;B
p�B
qB
qAB
qAB
qvB
qvB
qvB
q�B
q�B
q�B
rGB
rGB
r�B
r�B
r�B
sB
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
uZB
u�B
u�B
u�B
u�B
uZB
u�B
u�B
u�B
u�B
v�B
w2B
w�B
x8B
x�B
y	B
y�B
z�B
z�B
z�B
|B
|�B
}"B
}VB
}�B
}�B
~(B
~]B
~]B
~]B
~]B
~(B
~]B
~�B
~�B
~�B
~�B
.B
cB
�B
�B
�B
�B
�4B
��B
�;B
�B
�B
�oB
�AB
��B
��B
��B
�B
�B
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
�%B
�%B
�%B
�%B
��B
��B
�fB
��B
��B
�7B
�7B
�lB
�7B
�lB
�lB
�lB
�lB
�7B
�lB
�lB
�lB
�	B
�=B
�rB
��B
��B
�B
�B
�B
�B
��B
�JB
�JB
�B
��B
��B
��B
��B
�"B
��B
��B
��B
��B
�(B
�\B
��B
��B
�.B
�.B
�bB
�bB
�bB
�bB
�bB
� B
�4B
�hB
��B
��B
��B
��B
��B
��B
�:B
�:B
��B
��B
�oB
�oB
�oB
�oB
�oB
��B
��B
��B
�@B
��B
��B
��B
�FB
�B
�FB
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�B
��B
�YB
��B
�1B
��B
��B
��B
�1B
�1B
�eB
�1B
�eB
��B
��B
��B
��B
�eB
�eB
�1B
��B
��B
��B
�7B
�7B
��B
�	B
�	B
�=B
�qB
�qB
��B
�B
�B
�B
�B
�B
�B
��B
�B
�CB
�CB
�B
�CB
�xB
��B
��B
��B
�B
�B
�~B
�~B
��B
��B
��B
��B
��B
�IB
��B
�B
�B
�OB
�B
��B
��B
��B
�VB
��B
��B
��B
��B
�'B
�'B
��B
�-B
�bB
��B
��B
�4B
�hB
�4B
��B
��B
��B
��B
�hB
��B
��B
�B
�:B
��B
��B
�B
�B
��B
�B
�tB
��B
��B
�zB
��B
��B
�zB
��B
��B
�B
��B
�B
�B
�B
�LB
��B
�LB
��B
��B
�B
��B
�$B
�$B
�$B
�XB
��B
��B
��B
�*B	�BJBB	�B�B�B	�BBB�BDBBDB�B�B
�B
=BBPB	lBJB�B
	BxBJBxBDB�B
=B�BJB
rBDBJBB
=BxBJB
rBJBJB	�B�BB
	BxBB	lB�B�B
	BJBxB
�BJB
�B
=B�BJB
	B
�BPB�BDB�B�B�B�BB	�BxB�B	lB�B�BBBxBDB
�B
	BMB~B�B�B�B�BB(B�B�B�B"B�B�B�B�B�B4B(B�BhB\B�BhB B(B4BhB�B.B�B�B�B�B�B�BhB�B(B.BbB�B�BbBbB�B�B�BVB�B�B�B�B�B(B�BVB\B�BPB�B�BBVB�BVB�B�B�B�B�B�B�BDBB�BB~B�B�B�B�B\B\BVB\B4B�B(B�B�B\B.B4BbB�BB:B BhB�B B�B:B�B4B�B�B B�BB�BoB@B�BhBB�B�B4B@BhB�B@B:B4BuBoB B�B:B�B�B�B�B�B�BoB�B:BB@B@BFB�B{B�B�B�B�BFB�B�BMBB�BB�B�B�B�BBuB�BB�B�BuB�B�BhB�B"BVBPB
=B
	BDB�B�B�B+B�B  B iB
�]B
�	B
�B
�B
�8B
�B
�/B
�)B
��B
�B
�B
�PB
�fB
�JB
��B
��B
��B
�.B
��B
��B(B�B�BkB�B�B�B�B
��B
�DB
�JB�B�B
��B
��B�BuB �BB�BGB�B�B%BSB�B
�BbB=B!�B&B0UB/B2aB5�BAUBD�BPBYBZ�BZBYB[#BZ�BYKBYKBZ�BY�BW�BY�BaBrBy>B}�B��B�SB��B�B��B�9B��B�jB��B�UBǮB�?BŢB�RB�0B�B�<BΥB�HB҉B�&B�TB� B��B�B՛B՛B�?B��B��B�/B��B�HB�B�8B�B��B�QB�B��B�B�"B� B�;B�B�5B� B�AB��B�oB��B�GB�B��B�oB�GB�B��B�`B�B�%B�%B�B�MB��B��B�B��B�B�B�B�B��B�vB��B�B�B��B�B��B��B��B��B��B��B�B�PB�"B��B�VB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�(B��B��B��B 4BoBGB�B�B�B�B�BMB�BBxBB�B��B��B�VB��B�>B�8B�B��B �B�.B_BBMB��B�JB�%B�+B��B�B��B�TB�2B�B�%B��B�B�B��B�B�/B�B��B�B�vB��B��B��B�vB�B�B��B�B�vB�;B�B�MB�vB��B��B�|B�B�oB��B�B�B�vB�|B�B�AB�oB�B��B�GB�%B��B��B�iB��B��B�WB��B��B��B��B�B�B�QB��B�B�B�B�B�B�B�B�DB�B�yB�B�B�B�B�
B��B�sB�B�yB�QB�B�KB�B�B�KB�B�DB�DB�
B��B�B�)B�vB�B�/B�QB�yB�B�B�B�"B�WB��B�KB�8B�B�B��B�BߤB�;BޞB�B�/B��BیBݘB�B�#BںBںB�B�yBخBٴB�#BٴB�EB�#B�BٴB�?B�pB�#B�)B�#B�]B��B�WB��B��B��B��B��B�B�/B�)B�dB�B�pB��B�B�vB�B�B�TB�BߤB�pB�5BߤB�BB��B�BB�BݘBܒB�B��B�TB��B�WB�,B�sB�mB�2B�ZB�B�fB��B�B��B�B�B�KB��B��B�iB�|B��B�B�B�mB�B�&B�B��B�,B�ZB�B��B�fB�B�yB� B�BB�BBݘB�B�dB�B��B��BخB�yB՛B�[B�9B՛B�gB�?B�
B�&B��B�B�BרB�KB�B̘B��B�B�BB��B�B�[B�HB��B�<B�jB��B��B��B��B��B�$B�zB��B�B��B��B�9B�-B��B�qB�B�B�XB��B�FB�FB��B�'B��B��B��B�bB�0B��B�*B�RB�B�B�~B��B��B�:B��B��B�	B�_B�B|PB|Bw2Bs�Bu�Bs�Bk�BiBjBj�BkB^�B[�B`�B_�Bb�BS[BM�BB�B>�B;�B>BB7�B7�B5tB33B6zB+�B,�B/B+kB*0B*eB.�B5B5tB1'B+�B(�B-�B(XB$B#�B�BeBBSBMB�B
=B+B�B�B	7BPB
=BMB�B
�B
�B
��B
��B
��B
�B
�;B
�GB
�B
��B
�%B
��B
�B
��B
�2B
�B
��B
��B
�DB
�NB
�BB
�B
�B
ݘB
�]B
��B
��B
�B
��B
֡B
�2B
��B
�pB
��B
ѷB
ںB
�TB
�B
�wB
�gB
��B
��B
��B
��B
��B
�@B
�@B
�B
��B
�eB
��B
�'B
�oB
�B
�B
��B
��B
� B
�	B
��B
�~B
�B
��B
�rB
�B
z�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022051717093020220517170930IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022052712011720220527120117QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022052712011720220527120117QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194520230210131945IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                